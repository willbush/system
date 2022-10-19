{ pkgs, ... }: {
  home = {
    # Confusingly, this needs to be bootstrapped by executing `krew install
    # krew` before `kubectl krew` works
    # Also, note that some plugins (such as rabbitmq) scripts need the interpreter changed to:
    #!/usr/bin/env bash
    packages = with pkgs; [ krew ];
    sessionPath = [ "$HOME/.krew/bin" ];
  };
}
