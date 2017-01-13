let scale = 1.0
// convert coordiantes to cartesian

let SphericalToCartesian (long:float, lat:float, r:float) =
  let degToRad (deg:float) = 
    (deg/360.0)*(2.0*System.Math.PI)
  let x = r*sin(degToRad(lat+90.0))*cos(degToRad(long))
  let y = r*sin(degToRad(lat+90.0))*sin(degToRad(long))
  let z = r*cos(degToRad(lat+90.0))
  (x,y,z)

type planet() =
    let mutable route = []
    let mutable pos = 0
    member this.setRoute(newroute) = route <- newroute
    member this.showRoute() = printfn "Planets route:\n %A" route
    member this.calculateSpeed() =
       match (route.[pos], route.[pos+1]) with
       | ((x1,y1,z1),(x2,y2,z2)) -> printfn "%A" (scale*(    sqrt ( (pown (x2-x1) 2)   +    (pown (y2-y1) 2)  + (pown (z2-z1) 2) )  )) 

       // bestem afstand her
    member this.test() = 
       printfn "%A\n%A\n%A\n%A" route.[0] route.[90] route.[180] route.[270]

    member this.loadDataFromFile (filename: string, coords:int) = 
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
                      // konverterer data og gemmer i listen  FILFORMAT: TID LONG LAT R

                      //printfn "%A %A %A %A" (float cleandata.[1]) cleandata.[2] cleandata.[3]
                      data <- data @ [(SphericalToCartesian((float cleandata.[1]), (float cleandata.[2]) ,(float cleandata.[3])))]    
                      ()
            else 
              ()
         // gem i planet     
        this.setRoute(data)    
        //printfn "%A" data      // clean data gemmes korrekt, men "data" virker ikke
        reader.Close()









//let Pluto = new planet()
//Pluto.showRoute()
//Pluto.calculateSpeed()
//dataToRoute (Pluto, "data/Pluto.txt", 1500)

let Earth = new planet()
Earth.loadDataFromFile("data/Earth.txt", 500)

Earth.showRoute()
//Earth.calculateSpeed()
printfn "testfunk"
Earth.test()


