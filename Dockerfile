FROM debian:jessie


ARG UID=1000
ENV USER="ubuntu"
RUN useradd -u $UID -ms /bin/bash $USER

RUN apt-get update -y
RUN apt-get install -y postgresql

RUN mkdir /mnt/ramdisk
RUN mount -t tmpfs -o size=2048m tmpfs /mnt/ramdisk

RUN initdb /mnt/ramdisk/pgdata
