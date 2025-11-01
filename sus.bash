#!/bin/bash

date; acpi -V | grep Battery;
echo locking.... ;
xscreensaver-command -lock ;

echo sleeping... ;
systemctl -i hybrid-sleep;
echo waiting... ;

# Record time before sleep
before=$(date +%s)

# Wait until time jumps forward significantly → resume detected
while true; do
    now=$(date +%s)
    diff=$(( now - before ))

    if (( diff > 5 )); then
        echo "System resumed (time jumped by $diff seconds)"
        break
    fi

    sleep 1
done

echo ...woken, clock shift detected; date; acpi -V | grep Battery

