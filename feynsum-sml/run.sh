./main.mpl @mpl procs 72 set-affinity megablock-threshold 14 cc-threshold-ratio 1.1 collection-threshold-ratio 2.0 max-cc-depth 1 -- -scheduler gfq -input $@
# cc-threshold-ratio 1.1 collection-threshold-ratio 2.0 


# ./all-main.mpl @mpl procs 72 set-affinity megablock-threshold 14 cc-threshold-ratio 1.25 max-cc-depth 1 -- -sim query-bfs -input $@
