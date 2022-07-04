# vim-plug
sh -c 'curl -fLo $HOME/.config/nvim/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# Alias vim/vi to nvim
if [ ! $(grep 'alias vim="nvim"' $HOME/.bashrc) ] && [ ! $(grep 'alias vi="nvim"' $HOME/.bashrc) ]; then
	echo 'alias vim="nvim"\nalias vi="nvim"' >> $HOME/.bashrc
	source $HOME/.bashrc
fi

# Install patched font
mkdir $HOME/.local/share/fonts
wget -cO - https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Hack.zip > Hack.zip
unzip Hack.zip -d -y "$HOME/.local/share/fonts/Hack/"
rm -f Hack.zip
