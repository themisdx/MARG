extensions [ csv matrix ]

links-own [ ; students have relationships with other students , the relationships are 2 directionals (are different in each direaction)
  attraction ; each student has an attraction towards each of the other students (attraction is reflected on a number from 0 to 1 with 1 the maximum and 0 the lowest)
  known-indices ; each student knows specific characteristics of the other student reflected in the known indices, each number in the area shows the position of the known characteristics
]

globals [
  ideal-external-characteristics ; cultural values and norms for external characteristics reflected in a discrete value
  ideal-internal-characteristics ; cultural values and norms for internal characteristics reflected in a discrete value

]

turtles-own [
  external-characteristics ; each student has an array of external characteristics - each item has a discrete value
  internal-characteristics ; each student has an array of internal characteristics - each item has a discrete value
  tolerance ; this number reflects half the range for accepting char as positive, with middle value the IDM
  #positiveinteractions ; the number of positive interactions for a student so far
  #negativeinteractions ; the number of negative interactions for a student so far
  #refusedinteractions ; the number of refused interactions for a student so far
  average_attraction_out ; the average of the attraction the agent receives
  stdev_attraction_out ; the st deviation of the attraction the agent receives
  average_attraction_in
  stdev_attraction_in
]

to setup
  clear-all
  set ideal-external-characteristics generate-characteristics num-external-characteristics Ex_ideal_chars 0 ; generate-characteristics num-external-characteristics Average_char Stdev_char ; initialise the values of external characteristics
  set ideal-internal-characteristics generate-characteristics num-internal-characteristics In_ideal_chars 0 ; generate-characteristics num-internal-characteristics Average_char Stdev_char ;  initialise the values of internal characteristics
  create-turtles num-students [ ; create the students
   set #positiveinteractions 0 ; initialise all counts to interactions to 0
   set #negativeinteractions 0
   set #refusedinteractions 0
    create-links-to other turtles [ ; create two directional relationships
      set attraction Attitude ; setting up initial attraction to other students
      set known-indices [] ; nothing is known for the other students
    ]
    set external-characteristics generate-characteristics num-external-characteristics Average_char Stdev_char ; create external characteristics for each student
    set internal-characteristics generate-characteristics num-internal-characteristics Average_char Stdev_char ; create internal characteristics for each student
    set tolerance precision (random-float Max_judg) 2  ;setting up tolerance value for each student
    setxy random-xcor random-ycor ; spread turtles throughout the environment
    set average_attraction_out []
    set stdev_attraction_out []
    set average_attraction_in []
    set stdev_attraction_in []
  ]
  reset-ticks
end


to learn-about [ other-turtle ] ;; learn characteristics during interaction
  let learned-indices n-of charlearned_interaction range length [ internal-characteristics ] of other-turtle ;; set the number of learned characteristics during interaction
  set known-indices remove-duplicates sentence known-indices learned-indices ;; remove duplicates
end


to-report generate-characteristics [ num-characteristics average stdev ] ;; general procedure to initialise values for arrays randomly from a normal distribution
  report n-values num-characteristics [ max ( list 0.0 min ( list 1.0 (precision ( random-normal average stdev ) 2 )) ) ]
end

to-report positive-interaction? [ other-turtle me ] ; evaluate the interaction based on the known characteristics of the interacting partner and the tolerance of the evaluating student,2 inputs: the other-turtle is the student who is evaluated and me is the evaluator
  let deviations-external (map
    [ [ char1 char2 ] -> abs (char1 - char2) ] ;; find the absolute value of the difference between ...
    ideal-external-characteristics ;.. the ideal value for a characteristic
    [external-characteristics] of other-turtle ;.. the value of the characteristic, this will be done for each item in the array
  )
  let evaluation-external sum map [ deviation-external -> ;; find evaluation of each characteristic of the other student , if tolerance> deviation --> positive else negative and sum the evaluations
    ifelse-value deviation-external <= ( [tolerance] of me ) [ 1 ] [ -1 ]
  ] deviations-external

learn-about other-turtle ; update the known indices about the others
let deviations-internal 0 ; initialise the local variable deviation internal
  let evaluation-internal 0 ; initialise the local variable evaluation internal
      foreach known-indices [ ; for the known internal characteristics
        num -> (set deviations-internal abs(item num [internal-characteristics] of other-turtle - item num [ideal-internal-characteristics] of other-turtle )) ; each item in known indices indicates the position in the array, we call this number num
        ; find the difference of the item in place num of internal charasterstics of the evaluee with the ideal characteristics of the evaluator (weird because ideal characteristics are global, I get error when I do not specify "of other-turtle")
        ifelse deviations-internal <= [tolerance] of me ; if the deviation for the characteristic is lower than the tolerance of the evaluator ..
        [set evaluation-internal evaluation-internal + 1][set evaluation-internal evaluation-internal - 1] ; increase the evaluation internal by 1, otherwise decrease by 1
      ]

  let evaluation evaluation-external + evaluation-internal ; find overall evaluation for the other student
    report evaluation >= 0 ;; report positive interaction if the overall evaluation is positive or 0
end

to-report link-attributes-csv ;register the attraction between 2 agents
  report csv:to-row
    fput ["who1" "who2" "attraction" ]
    [ (list [ who ] of end1 [ who ] of end2 attraction ) ] of links
end

to-report turtle-data1-csv ;register the internal characteristics of each agent
  report csv:to-row
  fput [ "who" "internal-characteristics"]
  [ (list who internal-characteristics) ] of turtles
end

to-report turtle-data2-csv ;register the external characteristics of each agent
  report csv:to-row
  fput [ "who" "external-characteristics" ]
  [ (list who  external-characteristics) ] of turtles
end

to-report turtle-data3-csv ;register the internal characteristics of each agent
  report csv:to-row
  fput [ "who" "#positiveinteractions" "#negativeinteractions" "#refusedinteractions" "average_attraction_out" "stdev_attraction_out" "tolerance" "average_attraction_in" "stdev_attraction_in" ]
  [ (list who #positiveinteractions #negativeinteractions #refusedinteractions average_attraction_out stdev_attraction_out tolerance average_attraction_in stdev_attraction_in) ] of turtles
end


to go
  if ticks = 100  [ stop ] ; 100 in one semester
  ask turtles [
    repeat (( num-students - 1 ) / 2 )[ ; each student is forced to interact with half the other students
    let other-turtle one-of other turtles ; choose randomly interaction partner for mandatory interaction
    let me self ; identify the turtle who is me
    ask out-link-to other-turtle [ ; the initiating student towards the interaction partner
        ifelse positive-interaction? other-turtle me [set attraction precision max ( list 0.0 min ( list 1.0 (attraction + Attraction_change))) 2 ; decides whether the interaction is positive and updates attraction and intearactions accordingly
                                                      ask me [ set #positiveinteractions #positiveinteractions + 1 ]]
                                                     [set attraction precision max ( list 0.0 min ( list 1.0 (attraction - Attraction_change))) 2
                                                      ask me [ set #negativeinteractions #negativeinteractions + 1 ]]
      ]

      ask in-link-from other-turtle [ ifelse positive-interaction? me other-turtle [set attraction precision max ( list 0.0 min ( list 1.0 (attraction + Attraction_change))) 2 ; ; the interaction partner towards the initiating student
                                                      ask other-turtle [ set #positiveinteractions #positiveinteractions + 1 ]] ; decides whether the interaction is positive and updates attraction and intearactions accordingly
                                                     [set attraction precision max ( list 0.0 min ( list 1.0 (attraction - Attraction_change))) 2
                                                      ask other-turtle [ set #negativeinteractions #negativeinteractions + 1 ]]
    ]
    ]
  ]

  ask turtles [ ; free interactions
     repeat (( num-students - 1 ) / 2 ) ; each turtle has the opportunity of interactions with half the students in the free time
    [
    let other-turtle one-of other turtles ; select randomly an interaction partner
    let me self ; indentify the initiating student
    let me_reject_interaction true ; initialise the student decision to interact to negative decision
    let other_reject_interaction true; initialise the partner decision to interact to negative decision

    ask out-link-to other-turtle [ ; the initiating towards the interaction partner
        ifelse attraction > random-float 1 [ set me_reject_interaction false  ] ; if attraction exceeds a random number initiate interaction
        [ set me_reject_interaction true ; if attraction is below a random number do not initiate interaction
   ] ]

    ask in-link-from other-turtle [ ;the interaction partner towards the initiator
        if  me_reject_interaction = false [ ; if the interaction is iniitiated
        ifelse attraction > random-float 1 ; the partner decides whether to proceed with the interaction based on attraction and a random number
        [ set other_reject_interaction false ; the interaction is accepted
      ifelse positive-interaction? me other-turtle [set attraction precision max ( list 0.0 min ( list 1.0 (attraction + Attraction_change))) 2 ; decide whether the interaction partner likes the interaction
                                                      ask other-turtle [ set #positiveinteractions #positiveinteractions + 1 ]] ;if interaction is positive increase attraction by means of attraction change
                                                     [set attraction precision max ( list 0.0 min ( list 1.0 (attraction - Attraction_change))) 2
                                                      ask other-turtle [ set #negativeinteractions #negativeinteractions + 1 ]] ;if interaction is negative decrease attraction by means of attraction change
        ]
        [ set other_reject_interaction true ] ; if interaction is refused the local variable registers the result
        ]
        ]
     if me_reject_interaction = false and other_reject_interaction = false [ ; if interaction is on
      ask out-link-to other-turtle [
      ifelse positive-interaction? other-turtle me [set attraction precision max ( list 0.0 min ( list 1.0 (attraction + Attraction_change))) 2 ; decide whether the initiator likes the interaction
                                                      ask me [ set #positiveinteractions #positiveinteractions + 1 ]] ; if the evaluation is positive count one positive intreaction and increase attraction
                                                     [set attraction precision max ( list 0.0 min ( list 1.0 (attraction - Attraction_change))) 2 ; if the evaluation is negative count one negative intreaction and decrease attraction
                                                      ask me [ set #negativeinteractions #negativeinteractions + 1 ]]

      ]]
    if me_reject_interaction = false and other_reject_interaction = true [ set #refusedinteractions #refusedinteractions + 1 ] ; if the interaction is refused by the second partner the initiator regsters one refused interaction

  ]
  ]
 if ticks = 99 [ calculate ] ; before the simulation stops calculate
  tick
end

to calculate
  ask turtles [
    let list_ofattraction_out []
    ask my-out-links [ set list_ofattraction_out fput attraction list_ofattraction_out ]
    set average_attraction_out precision ( mean list_ofattraction_out) 2
    set stdev_attraction_out precision ( standard-deviation list_ofattraction_out ) 2
    let list_ofattraction_in []
    ask my-in-links [ set list_ofattraction_in fput attraction list_ofattraction_in ]
    print list_ofattraction_in
    set average_attraction_in precision ( mean list_ofattraction_in) 2
    set stdev_attraction_in precision ( standard-deviation list_ofattraction_in ) 2
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
340
14
777
452
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
19
67
283
100
num-external-characteristics
num-external-characteristics
0
100
10.0
1
1
NIL
HORIZONTAL

BUTTON
22
183
96
217
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
21
125
194
158
num-students
num-students
0
100
40.0
1
1
NIL
HORIZONTAL

SLIDER
19
14
279
47
num-internal-characteristics
num-internal-characteristics
0
100
10.0
1
1
NIL
HORIZONTAL

BUTTON
127
180
191
214
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
27
251
199
284
Average_char
Average_char
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
28
302
200
335
Stdev_char
Stdev_char
0
0.5
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
29
353
201
386
Attitude
Attitude
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
28
411
200
444
Max_judg
Max_judg
0
0.5
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
27
474
208
507
charlearned_interaction
charlearned_interaction
0
20
1.0
1
1
NIL
HORIZONTAL

SLIDER
33
536
205
569
Attraction_change
Attraction_change
0
0.5
0.04
0.05
1
NIL
HORIZONTAL

PLOT
227
467
427
617
plot 1
NIL
NIL
0.0
10.0
0.0
5000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [#negativeinteractions] of turtles"
"pen-1" 1.0 0 -7500403 true "" "histogram [#positiveinteractions] of turtles"
"pen-2" 1.0 0 -2674135 true "" "histogram [#refusedinteractions] of turtles"

SLIDER
933
46
1105
79
In_ideal_chars
In_ideal_chars
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
940
118
1112
151
Ex_ideal_chars
Ex_ideal_chars
0
1
0.5
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment 1" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <steppedValueSet variable="Attraction_change" first="0.02" step="0.02" last="0.1"/>
    <steppedValueSet variable="Stdev_char" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-students" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Max_judg" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 2" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <steppedValueSet variable="Attraction_change" first="0.02" step="0.02" last="0.1"/>
    <steppedValueSet variable="Stdev_char" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-students" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Max_judg" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 3" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <steppedValueSet variable="Attraction_change" first="0.02" step="0.02" last="0.1"/>
    <steppedValueSet variable="Stdev_char" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-students" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Max_judg" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment test" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <enumeratedValueSet variable="Attraction_change">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stdev_char">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-students">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max_judg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment test" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <enumeratedValueSet variable="Attraction_change">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stdev_char">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-students">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max_judg">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>link-attributes-csv</metric>
    <metric>turtle-data1-csv</metric>
    <metric>turtle-data2-csv</metric>
    <metric>turtle-data3-csv</metric>
    <enumeratedValueSet variable="Attraction_change">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="In_ideal_chars">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stdev_char">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Average_char">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="charlearned_interaction">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-internal-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-external-characteristics">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max_judg">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-students">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ex_ideal_chars">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.25
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
