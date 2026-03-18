form Arguments
    sentence input_directory C:\Users\rober\Desktop\bi_coart_nasal\corpus
    sentence output_directory C:\Users\rober\Desktop\bi_coart_nasal\temp
    positive phone_tier 2
endform

# --- list of vowel labels ---
# Spanish vowels
spVowels$ = "a"

# English ARPABET 2-letter codes (without stress number)
enVowels$ = "AA"

# combine into a single space-separated string
vowelLabels$ = spVowels$ + " " + enVowels$

Create Strings as file list: "files", input_directory$ + "\*.TextGrid"
n = Get number of strings

for f from 1 to n
    selectObject: "Strings files"
    file$ = Get string: f
    
    Read from file: input_directory$ + "\" + file$
    tg = selected("TextGrid")
    
    # duplicate phone tier
    Duplicate tier: phone_tier, 3, "vowels"
    
    numIntervals = Get number of intervals: 3
    
    for i from 1 to numIntervals
        label$ = Get label of interval: 3, i
        
        # --- strip trailing digit if it exists ---
        if length(label$) > 0
            lastChar$ = mid$(label$, length(label$), 1)
            if lastChar$ >= "0" and lastChar$ <= "9"
                label$ = left$(label$, length(label$) - 1)
            endif
        endif
        
        # --- keep only if in vowel list ---
        found = index(vowelLabels$, label$)
        if found = 0
            Set interval text: 3, i, ""
        endif
    endfor
    
    Save as text file: output_directory$ + "\" + file$
    
    selectObject: tg
    Remove
endfor