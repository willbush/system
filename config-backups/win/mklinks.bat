set scriptPath=%~dp0

mklink %USERPROFILE%\.gitconfig %scriptPath%\config\.gitconfig
mklink %USERPROFILE%\.vsvimrc %scriptPath%\config\.vsvimrc

pause
