#!/bin/bash
# Early stop for the 1635 trims (2026-10-03, Hans): when trim t finishes with TS(own R) <= CRIT, kill every larger trim's fit
# (its grid_estimator processes, their fit subshell and run-1633-fit.sh instance). Smaller trims keep running. DRY=1 only prints.
CRIT=${CRIT:-27.59}; L=../Products/1635-oldmac-launch.log; TR="0.001 0.002 0.003 0.004 0.005"
pids_for() {   # grid_estimator PIDs of trim $1, then their parent and grandparent
  for p in $(ps -Ao pid,args | grep "grid_estimator" | grep "trim$1.csv" | grep -v grep | awk '{print $1}'); do
    pp=$(ps -o ppid= -p $p | tr -d ' '); gp=$(ps -o ppid= -p $pp | tr -d ' '); echo "$gp $pp $p"; done; }
killed=""
while :; do
  for t in $TR; do
    ts=$(grep "^done 1635-i-trim$t-" $L 2>/dev/null | sed 's/.*TS(R500) \([0-9.]*\).*/\1/')
    [ -z "$ts" ] && continue
    if awk -v a=$ts -v c=$CRIT 'BEGIN{exit !(a<=c)}'; then
      for u in $TR; do awk -v a=$u -v b=$t 'BEGIN{exit !(a>b)}' || continue
        case " $killed " in *" $u "*) continue;; esac
        P=$(pids_for $u); echo "$(date +%H:%M) trim $t passes (TS $ts <= $CRIT): stop trim $u -> pids $P"
        [ -z "$DRY" ] && [ -n "$P" ] && kill $P 2>/dev/null; killed="$killed $u"; done
    fi
  done
  [ -n "$DRY" ] && break
  grep -q "all done" $L 2>/dev/null && { echo "$(date +%H:%M) all done; stopped:$killed"; break; }
  sleep 60
done
