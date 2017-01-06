type planet() =

    let mutable localspeed = 0.0
    let mutable route = []

    member this.setRoute(newroute) =
       route <- newroute
    member this.showRoute() =
       printfn "Planets route:\n %A" route



// read text file
(*
let getPlanetSpeed (filename: string) = 
    // hent route[1] og [2] og find hastighed.


    // læs fra fil
    let reader = System.IO.File.OpenText filename
    let mutable input = ""
    while not(reader.EndOfStream) do 
        if reader.ReadLine () = "$$SOE" then

          let linje1 = reader.ReadLine().Split ' '
          let linje2 = reader.ReadLine().Split ' '
          let mutable koord1 = []
          let mutable koord2 = []
          for value in linje1 do
            if value.Length <> 0 then
              koord1 <- koord1 @ [value]
            else
              ()
          for value in linje2 do
            if value.Length <> 0 then
              koord2 <- koord2 @ [value]
            else
              ()

          printfn "%A" koord1
          printfn "%A" koord2
          
          ()
        else 
          ()
    reader.Close()
    // skriv til fil
    *)

//getPlanetSpeed "11g/data/Earth.txt"


// convert coordiantes to cartesian
let SphericalToCartesian (long:float, lat:float, r:float) =
  let x = r*sin(lat)*cos(long)
  let y = r*sin(lat)*sin(long)
  let z = r*cos(lat)
  (x,y,z)



let Earth = new planet()


let dataToRoute (planet:planet, filename: string, coords:int) = 
    let reader = System.IO.File.OpenText filename
    let mutable data = []
    while not(reader.EndOfStream) do 
        if reader.ReadLine () = "$$SOE" then
              // antal koordinater der gemmes
              for i=0 to coords-1 do
                  let mutable cleandata = []
                    // laver string til liste
                  let linje = reader.ReadLine().Split ' '
                  for value in linje do
                    // fjerner tomme elementer
                    if value.Length <> 0 then
                        cleandata <- cleandata @ [value]
                    else
                       ()
                  // konverterer data og gemmer i listen   
                  data <- data @ [(SphericalToCartesian((float cleandata.[1]), (float cleandata.[2]) ,(float cleandata.[3])))]    

                  ()
        else 
          ()
     // gem i planet     
    planet.setRoute(data)    
    //printfn "%A" data      // clean data gemmes korrekt, men "data" virker ikke
    reader.Close()


dataToRoute (Earth, "data/Earth.txt", 1500)

Earth.showRoute()




