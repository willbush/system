{
  xdg = {
    enable = true;
    mime.enable = true;
    mimeApps = {
      enable = true;
      # query mime type from a file like this:
      # xdg-mime query filetype your-file.extension
      # also check out:
      # https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
      defaultApplications = {
        "application/msword" = "writer.desktop";
        "application/pdf" = "org.pwmt.zathura.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "writer.desktop";

        "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";
        "application/x-mobipocket-ebook" = "com.github.johnfactotum.Foliate.desktop";
        "application/x-fictionbook+xml" = "com.github.johnfactotum.Foliate.desktop";

        "application/vnd.ms-excel" = "calc.desktop";
        "application/vnd.oasis.opendocument.spreadsheet" = "calc.desktop";
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "calc.desktop";

        "audio/aac" = "vlc.desktop";
        "audio/mpeg" = "vlc.desktop";
        "audio/ogg" = "vlc.desktop";
        "audio/wav" = "vlc.desktop";
        "audio/webm" = "vlc.desktop";
        "audio/x-midi" = "vlc.desktop";

        "image/gif" = "imv.desktop";
        "image/jpeg" = "imv.desktop";
        "image/jpg" = "imv.desktop";
        "image/png" = "imv.desktop";
        "image/svg+xml" = "firefox.desktop";
        "image/tiff" = "imv.desktop";
        "image/vnd.microsoft.icon" = "imv.desktop";
        "image/webp" = "imv.desktop";

        # Open directory in Emacs dired.
        "inode/directory" = "emacsclient.desktop";

        "text/html" = "firefox.desktop";
        "text/plain" = "emacsclient.desktop";

        "video/mp4" = "vlc.desktop";
        "video/mpeg" = "vlc.desktop";
        "video/ogg" = "vlc.desktop";
        "video/webm" = "vlc.desktop";
        "video/x-matroska" = "vlc.desktop";
        "video/x-msvideo" = "vlc.desktop";

        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
      };

      associations.added = {
        # These apps overwrite mimeapps.list on startup unless this is explicitly added
        "x-scheme-handler/magnet" = "transmission-gtk.desktop";
        "x-scheme-handler/tg" = "telegramdesktop.desktop";
      };
    };

    userDirs = {
      enable = true;
      desktop = "$HOME/desktop";
      download = "$HOME/downloads";
      publicShare = "$HOME/public";
      videos = "$HOME/videos";
    };
  };
}
