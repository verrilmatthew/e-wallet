      *================================================================*
      * PROGRAM    : EWALLET                                           *
      * DESCRIPTION: Sistem E-Wallet Sederhana dalam COBOL             *
      * AUTHOR     : Claude (Anthropic)                                *
      * FEATURES   : - Cek Saldo                                       *
      *              - Top Up Saldo                                     *
      *              - Transfer ke Pengguna Lain                        *
      *              - Tarik Tunai                                      *
      *              - Riwayat Transaksi (5 terakhir)                   *
      *================================================================*

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EWALLET.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2026-05-12.

      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. ANY-COMPUTER.
       OBJECT-COMPUTER. ANY-COMPUTER.

      *================================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- DATA PENGGUNA (simulasi 3 akun) ---*
       01 WS-USERS.
           05 WS-USER OCCURS 3 TIMES.
               10 WS-USER-ID         PIC 9(4).
               10 WS-USER-NAME       PIC X(20).
               10 WS-USER-PHONE      PIC X(13).
               10 WS-USER-BALANCE    PIC 9(10)V99.
               10 WS-USER-PIN        PIC 9(6).

      *--- RIWAYAT TRANSAKSI ---*
       01 WS-TRANSACTIONS.
           05 WS-TRX OCCURS 5 TIMES.
               10 WS-TRX-NO         PIC 9(8).
               10 WS-TRX-TYPE       PIC X(10).
               10 WS-TRX-AMOUNT     PIC 9(10)V99.
               10 WS-TRX-DATE       PIC X(10).
               10 WS-TRX-DESC       PIC X(30).

       01 WS-TRX-COUNT              PIC 9(2) VALUE 0.
       01 WS-TRX-POINTER            PIC 9(2) VALUE 1.

      *--- VARIABEL KONTROL ---*
       01 WS-CURRENT-USER           PIC 9(2) VALUE 0.
       01 WS-MENU-CHOICE            PIC 9(1).
       01 WS-CONTINUE-FLAG          PIC X VALUE 'Y'.
       01 WS-LOGIN-FLAG             PIC X VALUE 'N'.
       01 WS-INPUT-PHONE            PIC X(13).
       01 WS-INPUT-PIN              PIC 9(6).
       01 WS-INPUT-AMOUNT           PIC 9(10)V99.
       01 WS-INPUT-TARGET-PHONE     PIC X(13).
       01 WS-TARGET-USER-IDX        PIC 9(2) VALUE 0.
       01 WS-LOOP-IDX               PIC 9(2).
       01 WS-FOUND-FLAG             PIC X VALUE 'N'.
       01 WS-ERROR-MSG              PIC X(40).
       01 WS-CONFIRM                PIC X.
       01 WS-TRX-SEQUENCE           PIC 9(8) VALUE 10000001.
       01 WS-TODAY                  PIC X(10) VALUE '2026-05-12'.

      *--- FORMAT DISPLAY ---*
       01 WS-DISPLAY-BALANCE        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-DISPLAY-AMOUNT         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-DIVIDER                PIC X(50)
           VALUE '=================================================='.
       01 WS-SUBDIV                 PIC X(50)
           VALUE '--------------------------------------------------'.

      *================================================================*
       PROCEDURE DIVISION.

      *--- INISIALISASI DATA PENGGUNA ---*
       0000-INITIALIZE.
           MOVE 1001            TO WS-USER-ID(1)
           MOVE 'Budi Santoso'  TO WS-USER-NAME(1)
           MOVE '081234567890'  TO WS-USER-PHONE(1)
           MOVE 5000000.00      TO WS-USER-BALANCE(1)
           MOVE 123456          TO WS-USER-PIN(1)

           MOVE 1002            TO WS-USER-ID(2)
           MOVE 'Siti Rahayu'   TO WS-USER-NAME(2)
           MOVE '082345678901'  TO WS-USER-PHONE(2)
           MOVE 2500000.00      TO WS-USER-BALANCE(2)
           MOVE 654321          TO WS-USER-PIN(2)

           MOVE 1003            TO WS-USER-ID(3)
           MOVE 'Andi Wijaya'   TO WS-USER-NAME(3)
           MOVE '083456789012'  TO WS-USER-PHONE(3)
           MOVE 750000.00       TO WS-USER-BALANCE(3)
           MOVE 112233          TO WS-USER-PIN(3)

           PERFORM 0100-SHOW-WELCOME
           PERFORM 0200-LOGIN UNTIL WS-LOGIN-FLAG = 'Y'
           PERFORM 1000-MAIN-MENU UNTIL WS-CONTINUE-FLAG = 'N'
           STOP RUN.

      *--- TAMPILAN SELAMAT DATANG ---*
       0100-SHOW-WELCOME.
           DISPLAY ' '
           DISPLAY WS-DIVIDER
           DISPLAY '     *** COBOL E-WALLET v1.0 ***'
           DISPLAY '        Dompet Digital Anda'
           DISPLAY WS-DIVIDER
           DISPLAY ' '.

      *--- PROSES LOGIN ---*
       0200-LOGIN.
           DISPLAY 'Masukkan No. HP Anda : '
           ACCEPT WS-INPUT-PHONE
           DISPLAY 'Masukkan PIN (6 digit): '
           ACCEPT WS-INPUT-PIN

           MOVE 'N' TO WS-FOUND-FLAG
           MOVE 0   TO WS-CURRENT-USER

           PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1
               UNTIL WS-LOOP-IDX > 3 OR WS-FOUND-FLAG = 'Y'
               IF WS-USER-PHONE(WS-LOOP-IDX) = WS-INPUT-PHONE
                   IF WS-USER-PIN(WS-LOOP-IDX) = WS-INPUT-PIN
                       MOVE WS-LOOP-IDX TO WS-CURRENT-USER
                       MOVE 'Y'         TO WS-FOUND-FLAG
                       MOVE 'Y'         TO WS-LOGIN-FLAG
                   ELSE
                       DISPLAY '*** PIN SALAH! Coba lagi. ***'
                       MOVE 'Y' TO WS-FOUND-FLAG
                   END-IF
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = 'N'
               DISPLAY '*** No. HP tidak terdaftar! ***'
           END-IF

           IF WS-LOGIN-FLAG = 'Y'
               DISPLAY ' '
               DISPLAY 'Login berhasil! Selamat datang, '
                   WS-USER-NAME(WS-CURRENT-USER)
               DISPLAY ' '.

      *--- MENU UTAMA ---*
       1000-MAIN-MENU.
           DISPLAY WS-DIVIDER
           DISPLAY '         MENU UTAMA E-WALLET'
           DISPLAY WS-SUBDIV
           DISPLAY '  1. Cek Saldo'
           DISPLAY '  2. Top Up Saldo'
           DISPLAY '  3. Transfer'
           DISPLAY '  4. Tarik Tunai'
           DISPLAY '  5. Riwayat Transaksi'
           DISPLAY '  0. Keluar'
           DISPLAY WS-SUBDIV
           DISPLAY 'Pilih menu [0-5]: '
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM 2000-CEK-SALDO
               WHEN 2 PERFORM 3000-TOP-UP
               WHEN 3 PERFORM 4000-TRANSFER
               WHEN 4 PERFORM 5000-TARIK-TUNAI
               WHEN 5 PERFORM 6000-RIWAYAT
               WHEN 0
                   DISPLAY ' '
                   DISPLAY 'Terima kasih telah menggunakan COBOL E-Wallet!'
                   DISPLAY WS-DIVIDER
                   MOVE 'N' TO WS-CONTINUE-FLAG
               WHEN OTHER
                   DISPLAY '*** Pilihan tidak valid! ***'
           END-EVALUATE.

      *--- 2000: CEK SALDO ---*
       2000-CEK-SALDO.
           MOVE WS-USER-BALANCE(WS-CURRENT-USER)
               TO WS-DISPLAY-BALANCE
           DISPLAY ' '
           DISPLAY WS-SUBDIV
           DISPLAY '   INFORMASI SALDO'
           DISPLAY WS-SUBDIV
           DISPLAY '  Nama    : ' WS-USER-NAME(WS-CURRENT-USER)
           DISPLAY '  No. HP  : ' WS-USER-PHONE(WS-CURRENT-USER)
           DISPLAY '  Saldo   : Rp ' WS-DISPLAY-BALANCE
           DISPLAY WS-SUBDIV
           DISPLAY ' '.

      *--- 3000: TOP UP ---*
       3000-TOP-UP.
           DISPLAY ' '
           DISPLAY WS-SUBDIV
           DISPLAY '   TOP UP SALDO'
           DISPLAY WS-SUBDIV
           DISPLAY 'Masukkan jumlah Top Up (Rp): '
           ACCEPT WS-INPUT-AMOUNT

           IF WS-INPUT-AMOUNT <= 0
               DISPLAY '*** Jumlah tidak valid! ***'
           ELSE IF WS-INPUT-AMOUNT > 10000000
               DISPLAY '*** Maksimal Top Up Rp 10.000.000 ***'
           ELSE
               MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT
               DISPLAY 'Top Up sebesar Rp ' WS-DISPLAY-AMOUNT
               DISPLAY 'Konfirmasi? (Y/N): '
               ACCEPT WS-CONFIRM

               IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y'
                   ADD WS-INPUT-AMOUNT
                       TO WS-USER-BALANCE(WS-CURRENT-USER)
                   PERFORM 9000-RECORD-TRX-TOPUP
                   MOVE WS-USER-BALANCE(WS-CURRENT-USER)
                       TO WS-DISPLAY-BALANCE
                   DISPLAY '*** Top Up BERHASIL! ***'
                   DISPLAY '  Saldo baru: Rp ' WS-DISPLAY-BALANCE
               ELSE
                   DISPLAY '  Top Up dibatalkan.'
               END-IF
           END-IF
           DISPLAY ' '.

      *--- 4000: TRANSFER ---*
       4000-TRANSFER.
           DISPLAY ' '
           DISPLAY WS-SUBDIV
           DISPLAY '   TRANSFER'
           DISPLAY WS-SUBDIV
           DISPLAY 'No. HP Tujuan: '
           ACCEPT WS-INPUT-TARGET-PHONE

           MOVE 'N' TO WS-FOUND-FLAG
           MOVE 0   TO WS-TARGET-USER-IDX

           PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1
               UNTIL WS-LOOP-IDX > 3 OR WS-FOUND-FLAG = 'Y'
               IF WS-USER-PHONE(WS-LOOP-IDX) = WS-INPUT-TARGET-PHONE
                   MOVE WS-LOOP-IDX TO WS-TARGET-USER-IDX
                   MOVE 'Y' TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

           IF WS-FOUND-FLAG = 'N'
               DISPLAY '*** No. HP tujuan tidak ditemukan! ***'
           ELSE IF WS-TARGET-USER-IDX = WS-CURRENT-USER
               DISPLAY '*** Tidak bisa transfer ke diri sendiri! ***'
           ELSE
               DISPLAY 'Tujuan: ' WS-USER-NAME(WS-TARGET-USER-IDX)
               DISPLAY 'Jumlah Transfer (Rp): '
               ACCEPT WS-INPUT-AMOUNT

               IF WS-INPUT-AMOUNT <= 0
                   DISPLAY '*** Jumlah tidak valid! ***'
               ELSE IF WS-INPUT-AMOUNT >
                   WS-USER-BALANCE(WS-CURRENT-USER)
                   DISPLAY '*** Saldo tidak mencukupi! ***'
               ELSE
                   MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT
                   DISPLAY 'Transfer Rp ' WS-DISPLAY-AMOUNT
                   DISPLAY 'ke ' WS-USER-NAME(WS-TARGET-USER-IDX)
                   DISPLAY 'Konfirmasi? (Y/N): '
                   ACCEPT WS-CONFIRM

                   IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y'
                       SUBTRACT WS-INPUT-AMOUNT FROM
                           WS-USER-BALANCE(WS-CURRENT-USER)
                       ADD WS-INPUT-AMOUNT TO
                           WS-USER-BALANCE(WS-TARGET-USER-IDX)
                       PERFORM 9100-RECORD-TRX-TRANSFER
                       MOVE WS-USER-BALANCE(WS-CURRENT-USER)
                           TO WS-DISPLAY-BALANCE
                       DISPLAY '*** Transfer BERHASIL! ***'
                       DISPLAY '  Saldo tersisa: Rp ' WS-DISPLAY-BALANCE
                   ELSE
                       DISPLAY '  Transfer dibatalkan.'
                   END-IF
               END-IF
           END-IF
           DISPLAY ' '.

      *--- 5000: TARIK TUNAI ---*
       5000-TARIK-TUNAI.
           DISPLAY ' '
           DISPLAY WS-SUBDIV
           DISPLAY '   TARIK TUNAI'
           DISPLAY WS-SUBDIV
           DISPLAY 'Jumlah Penarikan (Rp): '
           ACCEPT WS-INPUT-AMOUNT

           IF WS-INPUT-AMOUNT <= 0
               DISPLAY '*** Jumlah tidak valid! ***'
           ELSE IF WS-INPUT-AMOUNT >
               WS-USER-BALANCE(WS-CURRENT-USER)
               DISPLAY '*** Saldo tidak mencukupi! ***'
           ELSE IF WS-INPUT-AMOUNT > 5000000
               DISPLAY '*** Maks penarikan Rp 5.000.000 ***'
           ELSE
               MOVE WS-INPUT-AMOUNT TO WS-DISPLAY-AMOUNT
               DISPLAY 'Tarik tunai Rp ' WS-DISPLAY-AMOUNT
               DISPLAY 'Konfirmasi? (Y/N): '
               ACCEPT WS-CONFIRM

               IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y'
                   SUBTRACT WS-INPUT-AMOUNT FROM
                       WS-USER-BALANCE(WS-CURRENT-USER)
                   PERFORM 9200-RECORD-TRX-WITHDRAWAL
                   MOVE WS-USER-BALANCE(WS-CURRENT-USER)
                       TO WS-DISPLAY-BALANCE
                   DISPLAY '*** Penarikan BERHASIL! ***'
                   DISPLAY '  Saldo tersisa: Rp ' WS-DISPLAY-BALANCE
               ELSE
                   DISPLAY '  Penarikan dibatalkan.'
               END-IF
           END-IF
           DISPLAY ' '.

      *--- 6000: RIWAYAT TRANSAKSI ---*
       6000-RIWAYAT.
           DISPLAY ' '
           DISPLAY WS-SUBDIV
           DISPLAY '   RIWAYAT TRANSAKSI (5 TERAKHIR)'
           DISPLAY WS-SUBDIV

           IF WS-TRX-COUNT = 0
               DISPLAY '  Belum ada transaksi.'
           ELSE
               PERFORM VARYING WS-LOOP-IDX FROM 1 BY 1
                   UNTIL WS-LOOP-IDX > WS-TRX-COUNT
                   MOVE WS-TRX-AMOUNT(WS-LOOP-IDX)
                       TO WS-DISPLAY-AMOUNT
                   DISPLAY WS-LOOP-IDX '. ['
                       WS-TRX-TYPE(WS-LOOP-IDX) '] '
                       WS-TRX-DATE(WS-LOOP-IDX)
                   DISPLAY '   ' WS-TRX-DESC(WS-LOOP-IDX)
                   DISPLAY '   Rp ' WS-DISPLAY-AMOUNT
                   DISPLAY '   No.Ref: ' WS-TRX-NO(WS-LOOP-IDX)
               END-PERFORM
           END-IF
           DISPLAY WS-SUBDIV
           DISPLAY ' '.

      *--- 9000: CATAT TRANSAKSI TOP UP ---*
       9000-RECORD-TRX-TOPUP.
           PERFORM 9900-SHIFT-TRX
           MOVE WS-TRX-SEQUENCE      TO WS-TRX-NO(1)
           MOVE 'TOP-UP'             TO WS-TRX-TYPE(1)
           MOVE WS-INPUT-AMOUNT      TO WS-TRX-AMOUNT(1)
           MOVE WS-TODAY             TO WS-TRX-DATE(1)
           MOVE 'Top Up Saldo'       TO WS-TRX-DESC(1)
           ADD 1 TO WS-TRX-SEQUENCE.

      *--- 9100: CATAT TRANSAKSI TRANSFER ---*
       9100-RECORD-TRX-TRANSFER.
           PERFORM 9900-SHIFT-TRX
           MOVE WS-TRX-SEQUENCE      TO WS-TRX-NO(1)
           MOVE 'TRANSFER'           TO WS-TRX-TYPE(1)
           MOVE WS-INPUT-AMOUNT      TO WS-TRX-AMOUNT(1)
           MOVE WS-TODAY             TO WS-TRX-DATE(1)
           STRING 'Transfer ke '
               WS-USER-NAME(WS-TARGET-USER-IDX)(1:8)
               DELIMITED SIZE
               INTO WS-TRX-DESC(1)
           ADD 1 TO WS-TRX-SEQUENCE.

      *--- 9200: CATAT TRANSAKSI TARIK TUNAI ---*
       9200-RECORD-TRX-WITHDRAWAL.
           PERFORM 9900-SHIFT-TRX
           MOVE WS-TRX-SEQUENCE      TO WS-TRX-NO(1)
           MOVE 'TARIK'              TO WS-TRX-TYPE(1)
           MOVE WS-INPUT-AMOUNT      TO WS-TRX-AMOUNT(1)
           MOVE WS-TODAY             TO WS-TRX-DATE(1)
           MOVE 'Tarik Tunai'        TO WS-TRX-DESC(1)
           ADD 1 TO WS-TRX-SEQUENCE.

      *--- 9900: GESER DATA TRANSAKSI (LIFO BUFFER) ---*
       9900-SHIFT-TRX.
           IF WS-TRX-COUNT < 5
               ADD 1 TO WS-TRX-COUNT
           END-IF
           PERFORM VARYING WS-LOOP-IDX FROM 5 BY -1
               UNTIL WS-LOOP-IDX <= 1
               MOVE WS-TRX-NO(WS-LOOP-IDX - 1)
                   TO WS-TRX-NO(WS-LOOP-IDX)
               MOVE WS-TRX-TYPE(WS-LOOP-IDX - 1)
                   TO WS-TRX-TYPE(WS-LOOP-IDX)
               MOVE WS-TRX-AMOUNT(WS-LOOP-IDX - 1)
                   TO WS-TRX-AMOUNT(WS-LOOP-IDX)
               MOVE WS-TRX-DATE(WS-LOOP-IDX - 1)
                   TO WS-TRX-DATE(WS-LOOP-IDX)
               MOVE WS-TRX-DESC(WS-LOOP-IDX - 1)
                   TO WS-TRX-DESC(WS-LOOP-IDX)
           END-PERFORM.
