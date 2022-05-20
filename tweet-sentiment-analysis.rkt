#lang racket

;;; Tweets were obtained using twitter's academic api(tool used twarc2) in json format. 12 files from may 2021-22 were obtained converted to csv
;;; CSVs wered joined together, sorted by date, mofdified to remove empty rows, reduce size by removing low priority columns with python's pandas library
;;; data-science to read csv, process the text, and plot to visualize the
;;; results 
(require data-science csv-reading math math/matrix plot)

;;; Read in the entire tweet database (1,492,722  for Uganda tweets based on locality information provided by Twitter API)
(define tweets-data (read-csv "D:/tweet/Uganda/converted/05may21_05may22.csv" #:->number? #f #:header?  #t))

;;; select each tweet from the text id and make it into a list
;;; remove Retweets to avoid duplicates
(define tweet-text->lists
  (let ([tmp (map (λ (x) (list  (list-ref x 3))) tweets-data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))

;;; convert tweet-text from lists into strings
;;; append strings into one whole string
(define tweet-list->string-list
  (list (apply string-append (flatten tweet-text->lists))))

;;; use foldr which is better at traversing large/infinite lists
;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list
(define (preprocess-text lst)
  foldr (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))lst)


;;; preprocess the string to remove punctuation,urls etc.
;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the tweets
(define words (document->tokens (car (preprocess-text tweet-list->string-list)) #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))


;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;'(("anticipation" 399733)
;  ("joy" 361043)
;  ("positive" 723480)
;  ("surprise" 170085)
;  ("trust" 429511)
;  ("anger" 159969)
;  ("disgust" 103662)
;  ("fear" 202179)
;  ("negative" 330770)
;  ("sadness" 172198))

;;;Visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))