# INSTALLATION #

```bash
cd ~/
git clone https://github.com/willbush/dotfiles
```
Install symlinks:

```bash
cd ~/dotfiles
chmod +x init_symlinks.sh
./init_symlinks.sh
```

## ZSH ###

The zshrc config file for zsh shell is using the oh-my-zsh framework.
Installation goes as follows:

Install zsh, for example: `sudo apt install zsh`

Zsh install script will prompt you with some options, choose the one that does
not install a .zshrc or just be aware that installing this may overwrite the
symlink to the zshrc file from the previous step.

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

## KEY SWAP ###

After trying a few setups to make escaping insert mode easier in VIM and VIM IDE
plugins, I decided on swapping capslock and escape. I was trying to xcape, but
the delay in giving a key like shift or ctrl multi function was just too much.

```bash
sudo vim /etc/default/keyboard
```
Find XKBOPTIONS and add `caps:swapescape`, for example:
```bash
XKBOPTIONS="terminate:ctrl_alt_bksp, caps:swapescape"
```
then run the following command and go through the prompts:
```bash
sudo dpkg-reconfigure keyboard-configuration
```

That's all, it should work without having to logout or restart. To change it
back remove the `caps:swapescape` from that file and reconfigure again.
