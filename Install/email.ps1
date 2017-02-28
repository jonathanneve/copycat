$EmailFrom = “jonathan@copycat.fr”
$EmailTo = “jn@microtec.fr”
$Subject = “The subject of your email”
$Body = “What do you want your email to say”
$SMTPServer = “smtp.yaziba.net”
$SMTPClient = New-Object Net.Mail.SmtpClient($SmtpServer, 587)
$SMTPClient.EnableSsl = $true
$SMTPClient.Credentials = New-Object System.Net.NetworkCredential(“jn@microtec.fr”, “nahtanoj”);
$SMTPClient.Send($EmailFrom, $EmailTo, $Subject, $Body)