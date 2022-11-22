{ pkgs, ... }:
let sdk = pkgs.dotnet-sdk_7;
in
{
  home = {
    packages = [
      # note that Rider needs it's path to the dotnet cli tool fixed every time
      # this updates.
      sdk
    ];
    sessionVariables = {
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      # I thought this would help Rider found dotnet, but it doesn't seem to work.
      DOTNET_ROOT = sdk;
    };

    # Add dotnet tools to path
    sessionPath = [ "$HOME/.dotnet/tools" ];
  };
}
