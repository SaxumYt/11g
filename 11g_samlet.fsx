open System.Windows.Forms
open System.Drawing


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



let scale = 4.0 //Scale incase the model needs resizing.


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
    let mutable delta = 100.0
    member this.Delta
        with get() = delta
        and set(value) = delta <- value
    member this.setRoute(newroute) = route <- newroute
    member this.showRoute() =
        for vector in route do
            printfn "%A" (vector.tupleConvert())

    member this.calculateStartSpeed() =
        match (route.[pos], route.[pos+1]) with
        | (vector1: Vector, vector2: Vector) -> vector2 - vector1

    member this.acceleration (t: float)=
        let position: Vector = this.position(t)
        let value = 0.0002959122082322128/(pown (position.absoluteVal()) 3)
        value * position

    member this.position (t:float) =
        match t with
        | y when y <= 0.0 -> route.[0]
        | x -> this.position(t-delta) + (delta*this.velocity(t-delta))

    member this.velocity (t:float) =
        match t with
        | y when y <= 0.0 -> this.calculateStartSpeed()
        | x -> this.velocity(t-delta) + (delta*this.acceleration(t-delta))
    

    member this.test() =
        printfn "%A \n%A" (this.position(0.0).tupleConvert()) (route.[0].tupleConvert())
        printfn "%A \n%A" (this.position(1.0).tupleConvert()) (route.[1].tupleConvert())
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





// create planet objects

let Mercury = new planet()
let Venus = new planet()
let Earth = new planet()
let Mars = new planet()
let Jupiter = new planet()
let Saturn = new planet()
let Uranus = new planet()
let Neptune = new planet()
let Pluto= new planet()

// load data into planet objects 
Mercury.loadDataFromFile("data/Mercury.txt")
Venus.loadDataFromFile("data/Venus.txt")
Earth.loadDataFromFile("data/Earth.txt")
Mars.loadDataFromFile("data/Mars.txt")
Jupiter.loadDataFromFile("data/Jupiter.txt")
Saturn.loadDataFromFile("data/Saturn.txt")
Uranus.loadDataFromFile("data/Uranus.txt")
Neptune.loadDataFromFile("data/Neptune.txt")
Pluto.loadDataFromFile("data/Pluto.txt")


////////// winwinwinforms   ///////////////////////////////////////////////////////////////

// create timer and btn object
let timer = new Timer()
let butan = new Button()

/// Create a form
let createForm backgroundColor (width, height) title =
  let win = new Form ()
  win.Text <- title
  win.BackColor <- backgroundColor
  win.ClientSize <- Size (width, height)
  win

// Setup drawing details
let title = "Universe Simulator"
let backgroundColor = Color.Black
let size = (800, 800)
let origo = (400.0,400.0) // center in float
// create window
let win = createForm backgroundColor size title

// timer counter & position:t for planets
let mutable tickCounter = 0
// maxruns (0 = forever)
let mutable maxRuns = 0   // 0 to Int32


// tegne funktion
/// DrawEllipse (x,y, heigh,width) 
/// x = int (origo.x + planet.x*scale)
let drawPlanets() =

    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightGray), 4.0F),(int ((fst origo)+Mercury.position((float tickCounter)).x*scale)),(int ((snd origo)+Mercury.position((float tickCounter)).y*scale)),4,4)) //Mercury
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Goldenrod), 4.0F),(int ((fst origo)+Venus.position(float tickCounter).x*scale)),(int ((snd origo)+Venus.position(float tickCounter).y*scale)),4,4)) //Venus
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.CornflowerBlue), 4.0F),(int ((fst origo)+Earth.position(float tickCounter).x*scale)),(int ((snd origo)+Earth.position(float tickCounter).y*scale)),4,4)) //Earf
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Maroon), 4.0F),(int ((fst origo)+Mars.position(float tickCounter).x*scale)),(int ((snd origo)+Mars.position(float tickCounter).y*scale)),4,4)) //Mars
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightCoral), 6.0F),(int ((fst origo)+Jupiter.position(float tickCounter).x*scale)),(int ((snd origo)+Jupiter.position(float tickCounter).y*scale)),6,6)) //Jupiter
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.DarkKhaki), 6.0F),(int ((fst origo)+Saturn.position(float tickCounter).x*scale)),(int ((snd origo)+Saturn.position(float tickCounter).y*scale)),6,6)) //Saturn
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightSkyBlue), 5.0F),(int ((fst origo)+Uranus.position(float tickCounter).x*scale)),(int ((snd origo)+Uranus.position(float tickCounter).y*scale)),5,5)) //Uranus
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.RoyalBlue), 5.0F),(int ((fst origo)+Neptune.position(float tickCounter).x*scale)),(int ((snd origo)+Neptune.position(float tickCounter).y*scale)),5,5)) //Neptune
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Tan), 4.0F),(int ((fst origo)+Pluto.position(float tickCounter).x*scale)),(int ((snd origo)+Pluto.position(float tickCounter).y*scale)),4,4)) //Pluto

// input box til antal dage der køres
let tbTime = new TextBox()
tbTime.Width <- 100
tbTime.Height <- 50
tbTime.Visible <- true
tbTime.Enabled <- true
tbTime.Text <- "365"
tbTime.Location <- new Point (10, 10)
win.Controls.Add tbTime

// button action
let clicky (e : System.EventArgs) =
    maxRuns <- ((int)tbTime.Text)
    timer.Enabled <- true
    butan.Enabled <- false
    tbTime.Enabled <- false

//button
butan.Text <- "GO!"
butan.Height <- 50
butan.Width <- 50
butan.Location <- new Point (10, 50)
butan.ForeColor <- Color.White
butan.Click.Add clicky
win.Controls.Add butan

// timer function to update time.
let updatePlanets (form : Form) timerEventMagic =
    tickCounter <- tickCounter + 1
    printfn "Day: %A" (float tickCounter)
    if maxRuns = 0 then
        ()
    elif tickCounter >= maxRuns then
        timer.Enabled <- false
        butan.Enabled <- true
        tbTime.Enabled <- true
        tickCounter <- 0
    form.Refresh()

// Add timer events
timer.Interval <- 100
timer.Enabled <- false
timer.Tick.Add (updatePlanets win)


// draw planets at start position on startup
drawPlanets()

Application.Run win
