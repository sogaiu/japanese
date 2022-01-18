(defn decode-2
  [bytes start]
  (def byte-0
    (get bytes start))
  (def byte-1
    (get bytes (inc start)))
  #
  (when (and (= (band 2r11000000 byte-0) 2r11000000)
             (= (band 2r10000000 byte-1) 2r10000000))
    (def left-byte
      (brshift (band 2r00011100 byte-0) 2))
    (def right-byte
      (+ (blshift (band 2r00000011 byte-0) 6)
         (band 2r00111111 byte-1)))
    (+ (blshift left-byte 8) right-byte)))
  
(comment

  (decode-2 @"\xC2\xA3" 0)
  # =>
  0xA3

  )

(defn decode-3
  [bytes start]
  (def byte-0
    (get bytes start))
  (def byte-1
    (get bytes (inc start)))
  (def byte-2
    (get bytes (+ start 2)))
  #
  (when (and (= (band 2r11100000 byte-0) 2r11100000)
             (= (band 2r10000000 byte-1) 2r10000000)
             (= (band 2r10000000 byte-2) 2r10000000))
    (def left-byte
      (+ (blshift (band 2r00001111 byte-0) 4)
         (brshift (band 2r00111100 byte-1) 2)))
    (def right-byte
      (+ (blshift (band 2r00000011 byte-1) 6)
         (band 2r00111111 byte-2)))
    (+ (blshift left-byte 8) right-byte)))
  
(comment

  (decode-3 @"\xE0\xA4\xB9" 0)
  # =>
  0x0939

  (decode-3 @"\xE2\x82\xAC" 0)
  # =>
  0x20AC

  (decode-3 @"\xED\x95\x9C" 0)
  # =>
  0xD55C

  (decode-3 @"\xE4\xB8\x7F" 0)
  # =>
  nil

  )

(defn decode-4
  [bytes start]
  (def byte-0
    (get bytes start))
  (def byte-1
    (get bytes (inc start)))
  (def byte-2
    (get bytes (+ start 2)))
  (def byte-3
    (get bytes (+ start 3)))
  #
  (when (and (= (band 2r11110000 byte-0) 2r11110000)
             (= (band 2r10000000 byte-1) 2r10000000)
             (= (band 2r10000000 byte-2) 2r10000000)
             (= (band 2r10000000 byte-3) 2r10000000))
    #
    (def left-byte
      (+ (blshift (band 2r00000111 byte-0) 2)
         (brshift (band 2r00110000 byte-1) 4)))
    (def middle-byte
      (+ (blshift (band 2r00001111 byte-1) 4)
         (brshift (band 2r00111100 byte-2) 2)))
    (def right-byte
      (+ (blshift (band 2r00000011 byte-2) 6)
         (band 2r00111111 byte-3)))
    (+ (blshift left-byte 16)
       (blshift middle-byte 8)
       right-byte)))
  
(comment

  (decode-4 @"\xF0\x90\x8D\x88" 0)
  # =>
  0x10348

  )

(defn codepoint-from-bytes
  ``
  Try to decode a codepoint from `bytes` starting at index `start`,
  assuming the encoding is UTF-8.

  On success, returns a tuple of the codepoint and an index position
  in `bytes` for a potential next character.

  On failure, returns nil.
  ``
  [bytes start]
  (def byte
    (get bytes start))
  (case (band 2r11110000 byte)
    # 2 bytes
    2r11000000
    (when-let [cp (decode-2 bytes start)]
      [cp (+ start 2)])
    # 3 bytes
    2r11100000
    (when-let [cp (decode-3 bytes start)]
      [cp (+ start 3)])
    # 4 bytes
    2r11110000
    (when-let [cp (decode-4 bytes start)]
      [cp (+ start 4)])
    # single byte or problem
    (if (= (band 2r10000000 byte)
           2r00000000)
      [byte (inc start)]
      (do
        (eprintf "unexpected bit pattern: %p" byte)
        nil))))

(comment

  (codepoint-from-bytes @"\x24" 0)
  # =>
  [0x24 1]

  (codepoint-from-bytes @"\xC2\xA3" 0)
  # =>
  [0xA3 2]

  (codepoint-from-bytes @"\xE4\xB8\x80" 0)
  # =>
  [0x4E00 3]

  (codepoint-from-bytes @"\xE4\xB8\x7F" 0)
  # =>
  nil
  
  (codepoint-from-bytes @"\xE0\xA4\xB9" 0)
  # =>
  [0x0939 3]

  (codepoint-from-bytes @"\xE2\x82\xAC" 0)
  # =>
  [0x20AC 3]

  (codepoint-from-bytes @"\xED\x95\x9C" 0)
  # =>
  [0xD55C 3]

  (codepoint-from-bytes @"\xF0\x90\x8D\x88" 0)
  # =>
  [0x10348 4]

  )

(defn cjk-unified-ideograph?
  [bytes start]
  (when-let [[code-point byte-pos]
             (codepoint-from-bytes bytes start)]
    (when (and (= (- byte-pos start) 3)
               (<= 0x4E00 code-point 0x9FAF))
      [code-point byte-pos])))
  
(comment

  (cjk-unified-ideograph? @"仕" 0)
  # =>
  [0x4ED5 3]

  # 仕
  (cjk-unified-ideograph? @"\xE4\xBB\x95" 0)
  # =>
  [0x4ED5 3]

  (cjk-unified-ideograph? @"一" 0)
  # =>
  [0x4E00 3]

  # 一
  (cjk-unified-ideograph? @"\xE4\xB8\x80" 0)
  # =>
  [0x4E00 3]

  (cjk-unified-ideograph? @"\xE4\xB8\x7F" 0)
  # =>
  nil

  )

(defn hiragana?
  [bytes start]
  (when-let [[code-point byte-pos]
             (codepoint-from-bytes bytes start)]
    (when (and (= (- byte-pos start) 3)
               (<= 0x3040 code-point 0x309F))
      [code-point byte-pos])))

(comment

  (hiragana? @"つ" 0)
  # =>
  [0x3064 3]

  # つ
  (hiragana? @"\xE3\x81\xA4" 0)
  # =>
  [0x3064 3]

  (hiragana? @"っ" 0)
  # =>
  [0x3063 3]

  # っ  
  (hiragana? @"\xE3\x81\xA3" 0)
  # =>
  [0x3063 3]

  # 一
  (hiragana? @"\xE4\xB8\x80" 0)
  # =>
  nil

  (hiragana? @"つっ" 3)
  # =>
  [0x3063 6]

  )

(defn katakana?
  [bytes start]
  (when-let [[code-point byte-pos]
             (codepoint-from-bytes bytes start)]
    (when (and (= (- byte-pos start) 3)
               (<= 0x30A0 code-point 0x30FF))
      [code-point byte-pos])))

(comment

  (katakana? @"ン" 0)
  # =>
  [0x30F3 3]
  
  # ン
  (katakana? @"\xE3\x83\xB3" 0)
  # =>
  [0x30F3 3]

  (katakana? @"イ" 0)
  # =>
  [0x30A4 3]

  # イ
  (katakana? @"\xE3\x82\xA4" 0)
  # =>
  [0x30A4 3]

  (katakana? @"\xE3\x81\xA3" 0)
  # =>
  nil

  (katakana? @"\xE4\xB8\x80" 0)
  # =>
  nil

  )

(defn hangul-syllable?
  [bytes start]
  (when-let [[code-point byte-pos]
             (codepoint-from-bytes bytes start)]
    (when (and (= (- byte-pos start) 3)
               (<= 0xAC00 code-point 0xD7AF))
      [code-point byte-pos])))

(comment

  (hangul-syllable? @"가" 0)
  # =>
  [0xAC00 3]

  # 가
  (hangul-syllable? @"\xEA\xB0\x80" 0)
  # =>
  [0xAC00 3]

  (hangul-syllable? @"삥" 0)
  # =>
  [0xC0A5 3]

  # 삥
  (hangul-syllable? @"\xEC\x82\xA5" 0)
  # =>
  [0xC0A5 3]

  )

(defn hangul-jamo?
  [bytes start]
  (when-let [[code-point byte-pos]
             (codepoint-from-bytes bytes start)]
    (when (and (= (- byte-pos start) 3)
               (<= 0x1100 code-point 0x11FF))
      [code-point byte-pos])))

(comment

  (hangul-jamo? @"ᄋ" 0)
  # =>
  [0x110B 3]

  # ᄋ
  (hangul-jamo? @"\xE1\x84\x8B" 0)
  # =>
  [0x110B 3]

  (hangul-jamo? @"ᇫ" 0)
  # =>
  [0x11EB 3]

  # ᇫ
  (hangul-jamo? @"\xE1\x87\xAB" 0)
  # =>
  [0x11EB 3]

  )

(defn process-with-constraint
  [filepath dirpath constraint]
  (def f
    (file/open filepath :r))
  (unless f
    (break nil))
  (def of-table @{})
  (defer (file/close f)
    (var buf @"")
    (while (file/read f :line buf)
      (def word (string/trimr buf))
      (def bytes-len (length word))
      (when (zero? (% bytes-len 3))
        (def word-len (/ bytes-len 3))
        (var cf (get of-table word-len))
        (unless cf
          (set cf
               (file/open (string dirpath "/" word-len ".txt") :w))
          (put of-table word-len cf))
        (var only-with-constraint true)
        (var byte-pos 0)
        (while (and (< byte-pos bytes-len)
                    only-with-constraint)
          (if-let [[_ next-pos] (constraint word byte-pos)]
            (set byte-pos next-pos)
            (set only-with-constraint false)))
        (when only-with-constraint
          (file/write cf word)
          (file/write cf "\n")))
      (buffer/clear buf)))
  (eachp [len cf] of-table
    (file/close cf))
  true)

