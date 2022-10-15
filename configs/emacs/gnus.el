(setq user-mail-address "evan@petousis.net"
      user-full-name "Evangelos Petousis"
      mail-host-address "petousis.net")

(setq gnus-select-method
 '(nnimap "icloud"
          (nnimap-address "imap.mail.me.com")
          (nnimap-server-port 993)
          (nnimap-stream ssl)
          (nnmail-expiry-wait immediate)))

(setq send-mailfunction 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.mail.me.com")
(setq smtpmail-smtp-service 587)
