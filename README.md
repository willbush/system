Using nelstrom's dotfile backup method see more about it here:

http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/

https://github.com/nelstrom/dotfiles

# Installation #

```bash
git clone https://github.com/willbush/dotfiles
```

Where possible, Vim plugins are installed as git submodules. Check these out by
running the commands:

```bash
cd dotfiles
git submodule init
git submodule update
```

Create symlinks:

```bash
ln -s ~/dotfiles/vim ~/.vim
ln -s ~/dotfiles/vim/vimrc ~/.vimrc
ln -s ~/dotfiles/gitconfig ~/.gitconfig
ln -s ~/dotfiles/gimp-2.8 ~/.gimp-2.8
ln -s ~/dotfiles/zshrc ~/.zshrc
```

## VIM ##

Everything VIM related is stored under dotfiles/vim.

## Adding Plugin Bundles ##

Vim plugins that are published on github can be installed as submodules. For
example, to install the [JavaScript bundle](https://github.com/pangloss/vim-javascript), follow these steps:

```bash
cd ~/dotfiles
git submodule add http://github.com/pangloss/vim-javascript.git vim/bundle/vim-javascript
```

This will update the `.gitmodules` file by appending something like:

```bash
[submodule "vim/bundle/vim-javascript"]
    path = vim/bundle/vim-javascript
    url = http://github.com/pangloss/vim-javascript.git
```
    
As well as checkout out the git repo into the
`vim/bundle/vim-javascript` directory. You can then commit these changes
as follows:

```bash
git add -A
git commit -m "Added the javascript bundle"
```

### ZSH ###

The zshrc config file for zsh shell is using the oh-my-zsh framework. Installation goes as follows:

Install zsh, for example: `sudo apt-get install zsh`

Zsh install script will prompt you with some options, choose the one that does not install a .zshrc.

Install oh-my-zsh:

```bash
git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
```

Set Zsh as your default shell:

```bash
chsh -s /bin/zsh
```

Log out and back in, then verify the switch with:

```bash
echo $SHELL
```

If the zshrc file was cloned and symlinked properly, everything should work.
    
