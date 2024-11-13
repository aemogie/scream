.PHONY: run clean watch
run: out
	 ./out
clean:
	rm -f out
	rm -f out.c
	rm -f channels.lock.scm.tmp

channels.lock.scm.tmp:
	guix time-machine -C channels.scm -- describe -f channels > build/channels.lock.scm.tmp || exit 1
# doesnt depend on channels.scm as you might need to update lockfile without updating channels
update-lockfile: channels.lock.scm.tmp
	rm channels.lock.scm
	mv channels.lock.scm.tmp channels.lock.scm
out.c: example.sc scream.scm
	guile -s scream.scm example.sc -o out.c
out: out.c
	gcc out.c -o out
