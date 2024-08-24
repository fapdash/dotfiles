#!/bin/sh

sudo apt-get install -y curl\
                 wget\
                 git\
                 unzip\
                 ripgrep\
                 mpv\
                 img2pdf\
                 pdftk-java\
                 texlive\
                 texlive-xetex\
                 texlive-latex-extra\
                 texlive-extra-utils\
                 texlive-lang-german\
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
                 cowsay\
                 pandoc\
                 libenchant-2-dev `# jinx (emacs spell checker) dependency`\
                 ledger\
                 libreoffice-core\
                 entr\
                 bat\
                 sshpass `# ansible dependency`\
                 wl-clipboard `# wl-copy, wl-paste`\
                 htop\
                 lsd\
                 psensor `# cpu/gpu temperature`
