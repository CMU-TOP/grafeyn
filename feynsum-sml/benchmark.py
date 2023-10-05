import sys, time, shutil, glob, argparse, psutil, pathlib, fnmatch, os, tempfile
import simplejson as json
from copy import deepcopy
sys.setrecursionlimit(150000)

from flexibench import table as T, benchmark as B, query as Q


def extract_key_value_pairs(file, keys):
    result = {}
    for line in file:
        line = line.strip()
        if line:
            parts = line.split()
            print('line')
            print(line)
            if len(parts) == 2:
                print('parts')
                print(key)
                key, value = parts
                if key in keys:
                    result[key] = value
    return result

#print(extract_key_value_pairs('/home/rainey/Downloads/mpl_outfile.txt',['time','input']))

def get_files_with_extension(folder_path, extension):
    file_list = []
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            if file.endswith(extension):
                file_list.append(os.path.join(root, file))
    return file_list

def extract_filenames(file_paths):
    filenames = []

    for file_path in file_paths:
        filename = file_path.split('/')[-1]
        filenames.append(filename)
        
        return filenames
                                
infiles = extract_filenames(get_files_with_extension('inputs', 'qasm'))
# infiles = [
#     "adder_n28.qasm"]
# infiles = [
#     "adder_n28.qasm",
#     "bv_n30_reordered.qasm",
#     "ghz_state_n23.qasm",
#     "ising_n26.qasm",
#     "ising_n26_reordered.qasm",
#     "knn_n31.qasm",
#     "qram_n20.qasm",
#     "qram_n20_reordered.qasm"
# ]

# Key types
# --------- 

benchmark_key = 'benchmark'
procs_key = 'procs'
procs = [1, 32, 64, 96, 112, 224]
#procs = [112]

ranked_command_line_arg_keys = {'procs': 1, 'input': 0}

# keys whose associated values are to be passed as environment
# variables
env_arg_keys = []
# keys that are not meant to be passed at all (just for annotating
# rows)
silent_keys = [ ]

prog_keys = [ benchmark_key ]

def is_prog_key(k):
    return (k in prog_keys)
def is_silent_key(k):
    return (k in silent_keys)
def is_env_arg_key(k):
    return (k in env_arg_keys) and not(is_prog_key(k))
def is_ranked_command_line_arg_key(k):
    return (k in ranked_command_line_arg_keys)
def is_command_line_arg_key(k):
    return not(is_silent_key(k)) and not(is_env_arg_key(k)) and not(is_prog_key(k)) and not(is_ranked_command_line_arg_key(k))

# Benmchmark runs
# ===============

#  given a row, specifies the path of the program to be run
def program_of_row(row):
    return row[benchmark_key]

def virtual_run_benchmarks_of_rows(rows):
    i = 1
    n = len(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = program_of_row,
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        print(B.string_of_benchmark_run(br_i))
        i += 1

rows = T.rows_of(
    T.mk_cross2(
        T.mk_table1(benchmark_key, 'feynsum'),
        # T.mk_append2(T.mk_table1(benchmark_key, 'feynsum'),
        #              T.mk_table1(benchmark_key, 'cirq')),
        T.mk_cross([T.mk_append([T.mk_table1('input', f) for f in infiles]),
                    T.mk_append([T.mk_table1('procs', p) for p in procs])])))
stats_info = {
    'TIMER_OUTFILE': {'results': [], 'tmpfile': 'timer.txt', 'jsonfile': 'timer.json'}
}

def run_benchmark(br, stats0 = stats_info,
                  cwd = None, timeout_sec = None, verbose = True):
    stats = deepcopy(stats0)
    br_i = deepcopy(br)
    # generate a temporary file in which to store the stats output
    for k in stats:
        stats_fd, stats_path = tempfile.mkstemp(suffix = '.json', text = True)
        stats[k]['path'] = stats_path
        os.close(stats_fd)
    # let the taskparts runtime know about the temporary file above
    br_i['benchmark_run']['env_args'] += [{'var': k, 'val': stats[k]['path']} for k in stats]
    # set up other taskparts parameters
    if verbose:
        print(B.string_of_benchmark_run(br))
    # run the benchmark
    br_o = B.run_benchmark(br_i, cwd, timeout_sec, verbose = True)
    # collect the stats output of the benchmark run
    for k in stats:
        results = []
        stats_path = stats[k]['path']
        if os.stat(stats_path).st_size != 0:
            with open(stats_path, 'r') as file:
                # mine the elapsed time from the stdout of the benchmark
                # i'm expecting it to be a line of the form "time t" for some float t
                q = {}
                keys = ['time']
                for line in file:
                    for key in keys:
                        if key in line:
                            # Split the line into key and value
                            parts = line.split()
                            if len(parts) >= 2:
                                found_key = parts[0]
                                value = parts[1]
                                if found_key == key:
                                    q[found_key] = value
        stats[k]['results'] = q
        # remove the temporary file we used for the stats output
        open(stats_path, 'w').close()
        os.unlink(stats_path)
    return {'stats': stats, 'trace': br_o}

def merge_dictionaries(dict1, dict2):
    merged_dict = dict1.copy()
    merged_dict.update(dict2)
    return merged_dict

traces_tmp_file = 'traces.txt'

def json_of_tmp_file(results_txt, results_json):
    with open(results_txt, 'r') as results:
        lines = results.readlines()
        ds = []
        for line in lines:
            ds.append(json.loads(line))
        with open(results_json, 'w') as results:
            results.write(json.dumps(ds, indent=2))
            print('Emitted ' + results_json)

def run_benchmarks():
    if os.path.exists(traces_tmp_file):
        os.remove(traces_tmp_file)
    for k,v in stats_info.items():
        if os.path.exists(v['tmpfile']):
            os.remove(v['tmpfile'])
    virtual_run_benchmarks_of_rows(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = program_of_row,
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        b = run_benchmark(br_i)
        timestamp = b['trace']['benchmark_run']['timestamp']
        with open(traces_tmp_file, 'a') as traces:
            traces.write(json.dumps(b['trace']) + '\n')
        for k,v in stats_info.items():
            with open(v['tmpfile'], 'a') as results:
                r = b['stats'][k]['results']
                r['timestamp'] = timestamp
                results.write(json.dumps(merge_dictionaries(row, r)) + '\n')
    for k,v in stats_info.items():
        json_of_tmp_file(v['tmpfile'], v['jsonfile'])
    json_of_tmp_file(traces_tmp_file, 'traces.json')

run_benchmarks()
