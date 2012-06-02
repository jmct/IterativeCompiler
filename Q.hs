
  and v_0 v_1 = case v_0 of 
      <0> -> Pack{0,0};
      <1> -> v_1
      ;
  
  map v_2 v_3 = case v_3 of 
      <1> -> Pack{1,0};
      <0> v_71 v_72 ->
        Pack{0,2} (v_2 v_71) (map v_2 v_72)
      ;
  
  append v_6 v_7 = case v_6 of 
      <1> -> v_7;
      <0> v_73 v_74 ->
        Pack{0,2} v_73 (append v_74 v_7)
      ;
  
  concatMap v_10 v_11 = case v_11
    of 
      <1> -> Pack{1,0};
      <0> v_75 v_76 ->
        append (v_10 v_75) (concatMap v_10 v_76)
      ;
  
  length v_14 = lengthAcc 0 v_14;
  
  lengthAcc v_15 v_16 = case v_16
    of 
      <1> -> v_15;
      <0> v_77 v_78 ->
        lengthAcc (1 + v_15) v_78
      ;
  
  nsoln v_19
    = length (gen v_19 v_19);
  
  gen v_20 v_21 = case v_21 == 0
    of 
      <1> ->
        Pack{0,2} Pack{1,0} Pack{1,0};
      <0> ->
        concatMap (gen1 v_20) (gen v_20 (v_21 - 1))
      ;
  
  gen1 v_22 v_23
    = concatMap (gen2 v_23) (toOne v_22);
  
  gen2 v_24 v_25 = case
      safe v_25 1 v_24 of 
      <1> ->
        Pack{0,2} (Pack{0,2} v_25 v_24) Pack{1,0};
      <0> -> Pack{1,0}
      ;
  
  safe v_26 v_27 v_28 = case v_28
    of 
      <1> -> Pack{1,0};
      <0> v_79 v_80 ->
        and (v_26 /= v_79) (and (v_26 /= (v_79 + v_27)) (and (v_26 /= (v_79 - v_27)) (safe v_26 (v_27 + 1) v_80)))
      ;
  
  toOne v_31 = case v_31 == 1 of 
      <1> -> Pack{0,2} 1 Pack{1,0};
      <0> ->
        Pack{0,2} v_31 (toOne (v_31 - 1))
      ;
  
  main  = nsoln 2

