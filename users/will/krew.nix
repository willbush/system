{ pkgs, ... }: {
  home = {
    packages = with pkgs; [ krew ];
    sessionPath = [ "$HOME/.krew/bin" ];
  };
}
