{ pkgs, ... }: {
  home = {
    # Confusingly, this needs to be bootstrapped by executing `krew install
    # krew` before `kubectl krew` works
    packages = with pkgs; [ krew ];
    sessionPath = [ "$HOME/.krew/bin" ];
  };
}
