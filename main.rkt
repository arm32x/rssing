#lang racket

(require json)
(require net/http-client)
(require srfi/19)
(require xml)

; The ~z specifier doesn't like the colon in ISO 8601 timezones.
(define rfc2822-date-format "~a, ~d ~b ~Y ~H:~M:~S ~z")

(define (parse-iso8601-date str)
  (let* ([template-string "~Y-~m-~dT~H:~M:~S~z"]
         [timezone-colon-regex #px"(?<=\\+\\d{2}):(?=\\d{2})"]
         [str-without-colon (string-replace str timezone-colon-regex "")])
    (string->date str-without-colon template-string)))

; The maximum number of <item> elements to include in the feed.
(define max-feed-items (make-parameter 20))
; The location where this feed is hosted, for <atom:link rel="self">.
(define self-link (make-parameter "https://minecraft-updates-rss-feed.arm32.ax/feed.rss"))

(define (get-java-patch-notes-jsexpr)
  (define-values (status headers input-port)
    (http-sendrecv
      "launchercontent.mojang.com"
      "/javaPatchNotes.json"
      #:ssl? #t
      #:version "1.1"
      #:method "GET"))
  (read-json input-port))

(define (get-version-manifest-jsexpr)
  (define-values (status headers input-port)
    (http-sendrecv
      "piston-meta.mojang.com"
      "/mc/game/version_manifest_v2.json"
      #:ssl? #t
      #:version "1.1"
      #:method "GET"))
  (read-json input-port))

(define (truncate-list lst len)
  (if (<= (length lst) len)
    lst
    (take lst len)))

; Converts a string to a "slug" as used in Minecraft.net article URLs. This
; isn't entirely accurate since article URLs on Minecraft.net appear to be
; manually created, but this should be good enough for simple cases.
(define (minecraft-slugify str)
  (list->string
    (map
      (lambda (char)
        (if (or (char-alphabetic? char) (char-numeric? char))
          char
          #\-))
      (string->list (string-downcase str)))))
  

; Generates an X-expression for a Minecraft version given that version's patch
; notes and version manifest JSON. Note that this does not accept the entire
; JSON responses, it requires the JSON data for the individual version.
(define (generate-item-xexpr patch-notes version-manifest)
  `(item
     (title ,(hash-ref patch-notes 'title))
     (link ,(string-append
              "https://www.minecraft.net/en-us/article/"
              (minecraft-slugify (hash-ref patch-notes 'title))))
     (description ,(hash-ref patch-notes 'body))
     (guid ([isPermaLink "false"]) ,(hash-ref patch-notes 'id))
     (pubDate ,(date->string
                 (parse-iso8601-date (hash-ref version-manifest 'releaseTime))
                 rfc2822-date-format))))

; Merges the patch notes and version manifest for each version, returning a list
; of pairs, and limits the result to (max-feed-items) versions.
(define (preprocess-versions patch-notes-all version-manifest-all)
  (let* ([patch-notes-entries
           (hash-ref patch-notes-all 'entries)]
         [patch-notes-truncated
           (truncate-list patch-notes-entries (max-feed-items))]
         [version-manifest-versions
           (hash-ref version-manifest-all 'versions)])
    (for/list ([patch-notes patch-notes-truncated])
      (cons
        patch-notes
        (findf (lambda (version) (equal?
                                   (hash-ref version 'id)
                                   (hash-ref patch-notes 'version)))
               version-manifest-versions)))))
                                   
(define (generate-feed-xexpr)
  `(rss ([version "2.0"]
         [xmlns:atom "http://www.w3.org/2005/Atom"])
     (channel
       (title "Minecraft Updates")
       (link "https://www.minecraft.net/en-us")
       (description "Patch notes for Minecraft: Java Edition snapshots and releases")
       (language "en-us")
       (lastBuildDate ,(date->string (current-date) rfc2822-date-format))
       (atom:link ([href ,(self-link)]
                   [rel "self"]
                   [type "application/rss+xml"]))
       ,@(for/list ([version-pair (preprocess-versions
                                    (get-java-patch-notes-jsexpr)
                                    (get-version-manifest-jsexpr))])
           (generate-item-xexpr (car version-pair) (cdr version-pair))))))

(define (write-feed-to-file feed-xexpr file-path)
  (let ([output-port (open-output-file
                       file-path
                       #:mode 'text
                       #:exists 'truncate/replace)])
    (display-xml/content
      (xexpr->xml feed-xexpr)
      output-port
      #:indentation 'scan)))

(write-feed-to-file
  (generate-feed-xexpr)
  "feed.rss")

; vim: sw=2 ts=2 et
