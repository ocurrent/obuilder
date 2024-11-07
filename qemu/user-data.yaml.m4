#cloud-config
users:
  - name: opam
    groups: [sudo]
    sudo: ALL=(ALL) NOPASSWD:ALL
    shell: /bin/bash
    ssh_authorized_keys:
      - ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA09mqKPpMJ4tyOpl4l+KTTl1DqjFT2mRD29HW8VwnmB root@alpha
runcmd:
  - echo "AcceptEnv=*" > /etc/ssh/sshd_config.d/acceptenv.conf
  - mkdir /tmp/opam
  - curl -L https://opam.ocaml.org/install.sh -o /tmp/opam/install.sh
  - chmod +x /tmp/opam/install.sh
  - (cd /tmp/opam && ./install.sh --download-only --dev)
  - mv /tmp/opam/opam-* /usr/bin/opam-dev
  - chmod +x /usr/bin/opam-dev
  - ln -s /usr/bin/opam-dev /usr/bin/opam
  - apt update
  - apt install build-essential unzip bubblewrap -y
  - su - opam -c "git clone https://github.com/ocaml/opam-repository"
  - su - opam -c "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing"
  - su - opam -c "rm -rf .opam/repo/default/.git"
  - su - opam -c "echo export OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes OPAMERRLOGLEN=0 OPAMPRECISETRACKING=1 >> .bashrc"
  - su - opam -c "opam switch create VERSION --packages=ocaml-base-compiler.VERSION"
  - su - opam -c "opam pin add -k version ocaml-base-compiler VERSION"
  - su - opam -c "opam install -y opam-depext"
  - su - opam -c "mkdir ~/src"
  - poweroff
