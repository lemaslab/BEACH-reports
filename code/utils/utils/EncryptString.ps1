# Create directory user profile if it doesn't already exist.
$passwordDir = "$($env:USERPROFILE)\DPAPI\passwords\$($env:computername)"
New-Item -ItemType Directory -Force -Path $passwordDir
    
# Prompt for password to encrypt
$account = Read-Host "Please enter a label for the text to encrypt.  This will be how you refer to the password in R.  eg. MYDB_MYUSER"
$SecurePassword = Read-Host -AsSecureString  "Enter password" | convertfrom-securestring | out-file "$($passwordDir)\$($account).txt"
    
# Check output and press any key to exit
Write-Host "Press any key to continue..."
$x = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")