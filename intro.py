#  (local [(define v (make-vector 26 0))
 #         (define (count-char x)
  #          (local [(define i (- (char->integer x) ASCII-A))]
   #           (if (<= 0 i 25) (vector-set! v i (+ 1 (vector-ref v i))) 0)))]
   

   def count_char(aloc, c):
      result = 0 
      for achar in aloc:
         if achar == c:
            result == result + 1
         return result
      
   ASCII_A = 65

   def int_to_char(i):
       return chr(i+ASCII_A)
          
   def frequency_analysis(aloc):
      result = [] #empty list creationnnnn
      for i in range(26):
         result.append(count_char(aloc, int_to_char(i))) #appending lists
         return result
         
   L0 = ["A" , "B" , "C" , "C" , "C"]