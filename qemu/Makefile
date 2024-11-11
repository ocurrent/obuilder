
clean:
	rm -f *.qcow2 *.iso

# Windows

windows: windows-server-2022-amd64-ocaml-4.14.qcow2 windows-server-2022-amd64-ocaml-5.2.qcow2 busybox.qcow2

windows-server-2022-amd64-ocaml-5.2.qcow2: windows-server-2022-amd64-ocaml-5.2.0.qcow2
	ln -sf $< $@

windows-server-2022-amd64-ocaml-4.14.qcow2: windows-server-2022-amd64-ocaml-4.14.2.qcow2
	ln -sf $< $@

windows-server-2022-amd64-ocaml-%.qcow2: unattend-%.iso virtio-win.ISO
	qemu-img create -f qcow2 $@ 40G
	qemu-img create -f qcow2 cache-ntfs.qcow2 20G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=pc -cpu host -display none -vnc :0 \
		-netdev user,id=net0,hostfwd=tcp::60022-:22 -device virtio-net,netdev=net0 \
		-drive file=$@,if=virtio \
		-drive file=cache-ntfs.qcow2,if=virtio \
		-drive file=SW_DVD9_Win_Server_STD_CORE_2022_2108.6_64Bit_English_DC_STD_MLF_X23-03231.ISO,media=cdrom \
		-drive file=$<,media=cdrom \
		-drive file=virtio-win.ISO,media=cdrom

unattend-%.iso: autounattend.xml.m4 id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe
	m4 -D VERSION=$* $< > autounattend.xml
	mkisofs -o $@ -r -J autounattend.xml id_ed25519.pub openssh-win64.msi opam-2.2.exe opam-dev.exe setup-x86_64.exe

opam-2.2.exe:
	curl -L https://github.com/ocaml/opam/releases/download/2.2.1/opam-2.2.1-amd64-windows.exe -o opam-2.2.exe

opam-dev.exe:
	curl -L https://github.com/ocaml/opam/releases/download/2.3.0-beta1/opam-2.3.0-beta1-amd64-windows.exe -o opam-dev.exe

openssh-win64.msi:
	curl -L https://github.com/PowerShell/Win32-OpenSSH/releases/download/v9.2.2.0p1-Beta/OpenSSH-Win64-v9.2.2.0.msi -o openssh-win64.msi

setup-x86_64.exe:
	curl -L https://www.cygwin.com/setup-x86_64.exe -o setup-x86_64.exe

virtio-win.ISO:
	curl -L https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/archive-virtio/virtio-win-0.1.262-2/virtio-win.iso -o virtio-win.ISO

# Ubuntu

ubuntu: ubuntu-noble-amd64-ocaml-4.14.qcow2 ubuntu-noble-amd64-ocaml-5.2.qcow2 ubuntu-noble-riscv64-ocaml-4.14.qcow2 ubuntu-noble-riscv64-ocaml-5.2.qcow2 busybox.qcow2

ubuntu-noble-%-ocaml-5.2.qcow2: ubuntu-noble-%-ocaml-5.2.0.qcow2
	ln -sf $< $@

ubuntu-noble-%-ocaml-4.14.qcow2: ubuntu-noble-%-ocaml-4.14.2.qcow2
	ln -sf $< $@

seed-%.iso: user-data.yaml.m4
	m4 -D VERSION=$* $< > user-data.yaml
	cloud-localds $@ user-data.yaml

ubuntu-noble-amd64-ocaml-%.qcow2: noble-server-cloudimg-amd64.qcow2 seed-%.iso
	qemu-img create -f qcow2 -b $< -F qcow2 $@ 20G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=pc -cpu host -display none -vnc :0 \
		-drive file=$@ \
		-drive file=seed-$*.iso,format=raw \
		-nic user,hostfwd=tcp::60022-:22

ubuntu-noble-riscv64-ocaml-%.qcow2: noble-server-cloudimg-riscv64.qcow2 seed-%.iso
	qemu-img create -f qcow2 -b $< -F qcow2 $@ 20G
	qemu-system-riscv64 -m 16G -smp 8 -machine type=virt -nographic \
		-bios /usr/lib/riscv64-linux-gnu/opensbi/generic/fw_jump.bin \
		-kernel /usr/lib/u-boot/qemu-riscv64_smode/uboot.elf \
		-drive file=$@,if=virtio \
		-drive file=seed-$*.iso,format=raw,if=virtio \
		-netdev user,id=net0,hostfwd=tcp::60022-:22 -device virtio-net-device,netdev=net0

.NOTINTERMEDIATE: $(wildcard noble-server-cloudimg-*.qcow2)

noble-server-cloudimg-%.qcow2: noble-server-cloudimg-%.img
	curl -C - -L https://cloud-images.ubuntu.com/noble/current/$< -o $@

noble-server-cloudimg-%.img: ;

# busybox

seed.iso: busybox.yaml
	cp $< user-data.yaml
	cloud-localds $@ user-data.yaml

busybox.qcow2: noble-server-cloudimg-amd64.qcow2 seed.iso
	qemu-img create -f qcow2 -b $< -F qcow2 $@ 20G
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=pc -cpu host -display none -vnc :0 \
		-drive file=$@ \
		-drive file=seed.iso,format=raw \
		-nic user,hostfwd=tcp::60022-:22

# OpenBSD

openbsd: openbsd-76-amd64-ocaml-4.14.2.qcow2 openbsd-76-amd64-ocaml-5.2.0.qcow2

openbsd-76-amd64-ocaml-5.2.qcow2: openbsd-76-amd64-ocaml-5.2.0.qcow2
	ln -sf $< $@

openbsd-76-amd64-ocaml-4.14.qcow2: openbsd-76-amd64-ocaml-4.14.2.qcow2
	ln -sf $< $@

openbsd-76-amd64-ocaml-%.qcow2: tftp/auto_install tftp/bsd.rd tftp/etc/boot.conf mirror%
	qemu-img create -f qcow2 $@ 24G
	qemu-img create -f qcow2 cache-ffs.qcow2 20G
	python3 -m http.server --directory mirror --bind 0.0.0.0 80 & pid=$$! && \
	qemu-system-x86_64 -m 16G -smp 8 -machine accel=kvm,type=pc -cpu host -display none -vnc :0 \
		-drive file=$@,if=virtio \
		-drive file=cache-ffs.qcow2,if=virtio \
		-nic user,model=virtio,tftp-server-name=10.0.2.2,tftp=tftp,bootfile=auto_install,hostfwd=tcp::60022-:22 ; \
	kill $$pid

tftp/auto_install:
	mkdir -p tftp
	curl -L https://cdn.openbsd.org/pub/OpenBSD/7.6/amd64/pxeboot -o tftp/auto_install

tftp/bsd.rd:
	mkdir -p tftp
	curl -L https://cdn.openbsd.org/pub/OpenBSD/7.6/amd64/bsd.rd -o tftp/bsd.rd

tftp/etc/boot.conf:
	mkdir -p tftp/etc
	echo boot tftp:/bsd.rd > tftp/etc/boot.conf

mirror%: install.site.m4 install.conf disklabel
	m4 -D VERSION=$* $< > install.site
	chmod +x install.site
	mkdir -p mirror/pub/OpenBSD/7.6/amd64
	tar -czf mirror/pub/OpenBSD/7.6/amd64/site76.tgz install.site
	for f in BUILDINFO SHA256.sig base76.tgz bsd bsd.mp bsd.rd comp76.tgz game76.tgz man76.tgz pxeboot xbase76.tgz xfont76.tgz xserv76.tgz xshare76.tgz ; do curl -C - -L https://cdn.openbsd.org/pub/OpenBSD/7.6/amd64/$$f -o mirror/pub/OpenBSD/7.6/amd64/$$f ; done
	cd mirror/pub/OpenBSD/7.6/amd64 && ls -l > index.txt
	cp install.conf disklabel mirror

