SET scriptPath=%~dp0

mklink %USERPROFILE%\.emacs %scriptPath%emacs\.emacs
mklink %USERPROFILE%\.gitconfig %scriptPath%.gitconfig
mklink %USERPROFILE%\.vsvimrc %scriptPath%.vsvimrc

pause
