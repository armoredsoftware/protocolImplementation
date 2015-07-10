module ExampleArmoredConfig where

import ProtoTypes


--the request to send
request = ReqLS [RequestItem OS Name,RequestItem VC Name]


--the conditions that the results must meet
conditions = Or (OneOf OS Name [ValString "Windows", ValString "Linux"])
                (And (Equals VC Name (ValString "UniVaccine"))
                     (GTETV VC Version (ValDouble 5.1))
                )

--my privacy policy

policy =[
          Reveal [(OS,[Name,Version])] 
           (Or (OneOf VC Name [ValString "Avast", ValString "Norton",
                              ValString "Hearsa", ValString "Who"])
               (Equals ID Name (ValString "Appraiser")))
              
           
        ] 

config = ArmoredConfig request conditions policy