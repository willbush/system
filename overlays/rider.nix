self: super:
{
  jetbrains = super.jetbrains // {
    rider = super.jetbrains.rider.overrideAttrs (_: rec {
      # versions >= 2022.2 crash for me on startup. I haven't had time to look into why yet.
      # Also note that for some reason Rider can't find .NET SDK, even with DOTNET_ROOT set.

      # Therefore, in Rider, I have to do Settings > Build, Execution,
      # Deployment > Tools and Build > .NET CLI executable path.
      # and set it to something like:
      # /nix/store/zzpcf8ysd51a82rfhbs1wpygi1hqi6wp-dotnet-sdk-6.0.400/dotnet
      # exact path can be found with `dotnet --info`
      version = "2022.1.2";
      name = "rider-${version}";

      src = super.fetchurl {
        url = "https://download.jetbrains.com/rider/JetBrains.Rider-${version}.tar.gz";
        sha256 = "4513e55d3db013986bdfda52854f89fb127a153f2588ae456d6e7a182987b741";
      };
    });
  };
}
