# Simple script that removes some of the bloatware that comes with windows 10.
# There are a lot of non-removable bloatware on Windows 10 that requires more
# effort to remove. Look elsewhere for scripts that remove things like One Drive etc.

$bloatwareToRemove =
  "*WindowsCamera*",
  "*WindowsMaps*",
  "*WindowsSoundRecorder*",
  "*WindowsAlarms*",
  "*WindowsFeedbackHub*",
  "*windowscommunicationsapps*",
  "*WindowsPhone*",
  "*BingNews*",
  "*BingFinance*",
  "*BingSports*",
  "*BingWeather*",
  "*ZuneVideo*",
  "*ZuneMusic*",
  "*SkypeApp*",
  "*MicrosoftOfficeHub*",
  "*Messaging*",
  "*Advertising.Xaml*",
  "*Office.OneNote*",
  "*OneConnect*",
  "*Getstarted*",
  "*Phone*",
  "*Sway*",
  "*Print3D*",
  "*Microsoft3DViewer*",
  "*3DBuilder*",
  "*NetworkSpeedTest*",
  "*RemoteDesktop*",
  "*Wallet*",
  "*BubbleWitch*",
  "*HiddenCity*",
  "*Disney*",
  "*MarchofEmpires*",
  "*CandyCrush*",
  "*MicrosoftSolitaireCollection*",
  "*RoyalRev*",
  "*PowerBI*",
  "*Eclipse*",
  "*AdobePhotoshopExpress*",
  "*Sling*",
  "*Duolingo*",
  "*PhototasticCollage*",
  "*Keeper*",
  "*SketchBook*",
  "*DolbyAccess*",
  "*Spotify*",
  "*Pandora*",
  "*twitter*",
  "*Facebook*",
  "*XboxApp*",
  "*windowsstore*",
  "*Netflix*",
  "*FitbitCoach*",
  "*StorePurchaseApp*",
  "*GetHelp*",
  "*XboxGamingOverlay*",
  "*Windows.Photos*",
  "*ScreenSketch*",
  "*People*",
  "*MixedReality.Portal*",
  "*XboxGameOverlay*",
  "*MicrosoftStickyNotes*",
  "*XboxIdentityProvider*",
  "*XboxSpeechToTextOverlay*",
  "*Xbox.TCUI*"

foreach ($element in $bloatwareToRemove) {
  $package = Get-AppxPackage -Name $element
  if ($package) {
    write-host("Attempting to remove: " + $element)
    Remove-AppxPackage -Package $package.PackageFullName -Verbose
  } else {
    write-host("Not installed: " + $element)
  }
}

Read-Host -Prompt "Press Enter to continue"