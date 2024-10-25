
all: windows-server-2022-x86_64-ocaml-4.14.img ubuntu-noble-x86_64-ocaml-4.14.img

clean:
	rm -f unattend.iso seed.iso ubuntu-noble-x86_64-ocaml-4.14.img windows-server-2022-x86_64-ocaml-4.14.img

# Windows

windows-server-2022-x86_64-ocaml-5.2.img: unattend-5.2.0.iso
	qemu-img create -f qcow2 windows-server-2022-x86_64-ocaml-5.2.img 40G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=q35 -drive file=windows-server-2022-x86_64-ocaml-5.2.img -drive file=SW_DVD9_Win_Server_STD_CORE_2022_2108.6_64Bit_English_DC_STD_MLF_X23-03231.ISO,media=cdrom -drive file=unattend-5.2.0.iso,media=cdrom -cpu host -display none -vnc :0 -nic user,hostfwd=tcp::60022-:22
	# -drive file=virtio-win.iso,media=cdrom

windows-server-2022-x86_64-ocaml-4.14.img: unattend-4.14.2.iso
	qemu-img create -f qcow2 windows-server-2022-x86_64-ocaml-4.14.img 40G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=q35 -drive file=windows-server-2022-x86_64-ocaml-4.14.img -drive file=SW_DVD9_Win_Server_STD_CORE_2022_2108.6_64Bit_English_DC_STD_MLF_X23-03231.ISO,media=cdrom -drive file=unattend-4.14.2.iso,media=cdrom -cpu host -display none -vnc :0 -nic user,hostfwd=tcp::60022-:22
	# -drive file=virtio-win.iso,media=cdrom

unattend-4.14.2.iso: autounattend.xml.m4 id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe
	m4 -D VERSION=4.14.2 autounattend.xml.m4 > autounattend.xml
	mkisofs -o unattend-4.14.2.iso -r -J autounattend.xml id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe

unattend-5.2.0.iso: autounattend.xml.m4 id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe
	m4 -D VERSION=5.2.0 autounattend.xml.m4 > autounattend.xml
	mkisofs -o unattend-5.2.0.iso -r -J autounattend.xml id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe

opam-2.2.exe:
	curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-x86_64-windows.exe -o opam-2.2.exe

opam-dev.exe:
	curl -L https://github.com/ocaml/opam/releases/download/2.3.0-beta1/opam-2.3.0-beta1-x86_64-windows.exe -o opam-dev.exe

openssh-win64.msi:
	curl -L https://github.com/PowerShell/Win32-OpenSSH/releases/download/v9.2.2.0p1-Beta/OpenSSH-Win64-v9.2.2.0.msi -o openssh-win64.msi

setup-x86_64.exe:
	curl -L https://www.cygwin.com/setup-x86_64.exe -o setup-x86_64.exe

virtio-win.iso:
	curl -L https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/archive-virtio/virtio-win-0.1.262-2/virtio-win.iso -o virtio-win.iso

# Ubuntu

seed.iso: user-data.yaml
	cloud-localds seed.iso user-data.yaml

ubuntu-noble-x86_64-ocaml-4.14.img: noble-server-cloudimg-amd64.img seed.iso
	qemu-img create -f qcow2 -b noble-server-cloudimg-amd64.img -F qcow2 ubuntu-noble-x86_64-ocaml-4.14.img 20G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=q35 -drive file=ubuntu-noble-x86_64-ocaml-4.14.img -drive file=seed.iso,format=raw -cpu host -display none -vnc :0 -nic user,hostfwd=tcp::60022-:22

noble-server-cloudimg-amd64.img:
	curl -L https://cloud-images.ubuntu.com/noble/current/noble-server-cloudimg-amd64.img -o noble-server-cloudimg-amd64.img

