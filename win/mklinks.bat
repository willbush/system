set scriptPath=%~dp0

mklink /J %USERPROFILE%\.emacs.d %scriptPath%..\stow\.emacs.d
mklink %USERPROFILE%\.gitconfig %scriptPath%\config\.gitconfig
mklink %USERPROFILE%\.vsvimrc %scriptPath%\config\.vsvimrc

pause
