#!/bin/sh

sudo apt-get install -y curl\
                 wget\
                 git\
                 unzip\
                 ripgrep\
                 mpv\
                 img2pdf\
                 pdftk-java\
                 texlive-full\
                 ocrmypdf\
                 shellcheck\
                 qimgv\
                 keepassxc\
                 gimp\
                 gparted\
                 peek\
                 ripgrep\
                 silversearcher-ag\
                 pdfgrep\
                 mg\
                 audacity\
                 calibre\
                 foliate\
                 mupdf\
                 ffmpeg\
                 imagemagick\
                 imagemagick-doc\
                 gpick\
                 mumble\
                 filezilla\
                 easyeffects\
                 meld\
                 libtool-bin `# vterm dependency (emacs)`\
                 w3m\
                 lynx\
                 cowsay\
                 pandoc\
                 libenchant-2-dev `# jinx (emacs spell checker) dependency`\
                 hunspell\
                 hunspell-tools\
                 hunspell-de-de `# german spellchecking for libre office`\
                 ledger\
                 libreoffice-core\
                 entr\
                 bat\
                 sshpass `# ansible dependency`\
                 wl-clipboard `# wl-copy, wl-paste`\
                 htop\
                 lsd\
                 psensor `# cpu/gpu temperature`\
                 signify-openbsd\
                 sigsum-go\
                 gnuplot\
                 dconf-editor\
                 dictd\
                 dict-gcide\
                 dict-wn\
                 dict-jargon\
                 dict\
                 dict-foldoc\
                 dict-freedict-deu-eng\
                 dict-freedict-eng-deu\
                 p7zip-full\
                 cloc\
                 vlc\
                 vlc-plugin-pipewire\
                 scrub\
                 inotify-tools\
                 flatpak\
                 gnome-software-plugin-flatpak\
                 ausweisapp2\
                 podman\
                 inkscape\
                 rustup\
                 reflex\
                 xchm\
                 ncdu

flatpak remote-add --if-not-exists --subset=verified flathub https://dl.flathub.org/repo/flathub.flatpakrepom

sudo snap install rickrack
