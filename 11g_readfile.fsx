type Vector (x:float, y:float, z:float) = class
    static member (+) (vector1: Vector, vector2: Vector)=
        Vector (vector1.x + vector2.x, vector1.y + vector2.y, vector1.z + vector2.z)

    static member (-) (vector1: Vector, vector2: Vector)=
        Vector (vector1.x - vector2.x, vector1.y - vector2.y, vector1.z - vector2.z)
    static member ( * ) (a, vector: Vector)=
        Vector (vector.x*a, vector.y*a, vector.z*a)

    static member ( * ) (vector1: Vector, vector2: Vector)=
        Vector (vector1.x * vector2.x, vector1.y * vector2.y, vector1.z * vector2.z)

    member this.absoluteVal() =
        sqrt(float (pown (this.x) 2) + float (pown (this.y) 2) + float (pown (this.z) 2))
    member this.tupleConvert() =
        (this.x, this.y, this.z)
    member this.x = x
    member this.y = y
    member this.z = z
end



let scale = 1.0
// convert coordiantes to cartesian

let SphericalToCartesian (long:float, lat:float, r:float) =
    let degToRad (deg:float) =
        (deg/360.0)*(2.0*System.Math.PI)
    let x = r*sin(degToRad(lat+90.0))*cos(degToRad(long))
    let y = r*sin(degToRad(lat+90.0))*sin(degToRad(long))
    let z = r*cos(degToRad(lat+90.0))
    Vector (x,y,z)

type planet() =
    let mutable route: Vector list = []
    let mutable pos = 0
    let mutable count = 0
    let mutable delta = 0.1
    member this.Delta
        with get() = delta
        and set(value) = delta <- value
    member this.setRoute(newroute) = route <- newroute
    member this.showRoute() =
        for vector in route do
            printfn "%A" (vector.tupleConvert())
(*
    member this.calculateStartSpeed() =
        match (route.[pos], route.[pos+1]) with
        | (vector1: Vector, vector2: Vector) -> vector2 - vector1

    member this.acceleration (t: float)=
        let position: Vector = this.position(t)
        let value = 2.9591/(pown (position.absoluteVal()) 3)
        value * position

    member this.position (t:float) =
        match t with
        | y when y <0.0 -> route.[0]
        | x -> this.position(t-delta) + (delta*this.velocity(t-delta))

    member this.velocity (t:float) =
        match t with
        | 0.0 -> this.calculateStartSpeed()
        | x -> this.velocity(t-delta) + (delta*this.position(t-delta))

*)
       // bestem afstand her
    member this.test() =
        printfn "%A" (route.Length)
        printfn "%A\n%A\n%A\n%A" (route.[0].tupleConvert()) (route.[90].tupleConvert()) (route.[180].tupleConvert()) (route.[270].tupleConvert())

    member this.loadDataFromFile (filename: string) =
        let reader = System.IO.File.OpenText filename
        let mutable data = []
        let mutable linje = [||]
        let mutable cleandata = []
        while not(reader.EndOfStream) do
            //Kør hele listen med data igennem
            if reader.ReadLine () = "$$SOE" then
                let mutable currentLine = reader.ReadLine()
                while not(currentLine = "$$EOE") do
                    linje <- [||]
                    cleandata <- []
                    // laver string til liste
                    linje <- currentLine.Split ' '
                    currentLine <- reader.ReadLine()
                    for value in linje do
                        // fjerner tomme elementer
                        if value.Length <> 0 then
                            cleandata <- cleandata @ [value]
                    data <- data @ [(SphericalToCartesian((float cleandata.[1]), (float cleandata.[2]) ,(float cleandata.[3])))]
                ()
            else
                ()
        this.setRoute(data)
        reader.Close()







//let Pluto = new planet()
//Pluto.showRoute()
//Pluto.calculateSpeed()
//dataToRoute (Pluto, "data/Pluto.txt", 1500)

let Earth = new planet()
Earth.loadDataFromFile("Earth.txt")

//Earth.showRoute()
//Earth.calculateSpeed()
printfn "testfunk"

Earth.test()
