.PHONY: home home-build home-reconfigure system system-build system-reconfigure

home: home-container

home-build:
	guix home -L . build ./config/home/home-config.scm

home-container:
	guix home -L . container ./config/home/home-config.scm

home-reconfigure:
	guix home -L . reconfigure ./config/home/home-config.scm

system: system-container

system-build:
	guix system -L . build ./config/systems/erasmus.scm

system-container:
	guix system -L . container ./config/systems/erasmus.scm

system-reconfigure:
	guix system -L . reconfigure ./config/systems/erasmus.scm

update: channels-update-lock

channels-update-lock:
	guix time-machine -C ./channels.scm -- \
	describe -f channels > ./channels-lock.scm
