{
  lib,
  ...
}:
{
  options.user = {
    name = lib.mkOption {
      type = lib.types.str;
      description = "The primary user's username";
      default = "";
    };
  };
}
