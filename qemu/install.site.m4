#!/bin/ksh
set -o errexit
echo "https://cdn.openbsd.org/pub/OpenBSD" > /etc/installurl
echo "permit nopass keepenv :wheel" >> /etc/doas.conf

cat <<EOF >> /etc/rc.firsttime
echo "AcceptEnv=*" >> /etc/ssh/sshd_config
echo "PermitUserEnvironment=yes" >> /etc/ssh/sshd_config
pkg_add curl-- gmake gtar-- gpatch unzip-- rsync-- git
/usr/local/bin/curl -L https://github.com/ocaml/opam/releases/download/2.3.0-rc1/opam-2.3.0-rc1-x86_64-openbsd -o /usr/bin/opam-2.3
chmod +x /usr/bin/opam-2.3
ln -s /usr/bin/opam-2.3 /usr/bin/opam
su - opam -c "echo OPAMYES=1 >> .ssh/environment"
su - opam -c "echo OPAMCONFIRMLEVEL=unsafe-yes >> .ssh/environment"
su - opam -c "echo OPAMERRLOGLEN=0 >> .ssh/environment"
su - opam -c "echo OPAMPRECISETRACKING=1 >> .ssh/environment"
su - opam -c "git clone https://github.com/ocaml/opam-repository"
su - opam -c "opam init -k local -a /home/opam/opam-repository --bare"
su - opam -c "rm -rf .opam/repo/default/.git"
su - opam -c "opam switch create VERSION --packages=ocaml-base-compiler.VERSION"
su - opam -c "opam pin add -k version ocaml-base-compiler VERSION"
su - opam -c "opam install -y opam-depext"
su - opam -c "mkdir src"
echo '/ * 100%' > /tmp/sd1
disklabel -Aw -T /tmp/sd1 sd1
newfs sd1a
mount /dev/sd1a /home/opam/.opam/download-cache
chown opam:opam /home/opam/.opam/download-cache
umount /home/opam/.opam/download-cache
shutdown -p +1
EOF

