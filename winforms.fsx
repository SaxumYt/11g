open System.Windows.Forms
open System.Drawing

let timer = new Timer()
let butan = new Button()

/// Create a form and add a paint function
let createForm backgroundColor (width, height) title =
  let win = new Form ()
  win.Text <- title
  win.BackColor <- backgroundColor
  win.ClientSize <- Size (width, height)
  win

// Setup drawing details
let title = "title"
let backgroundColor = Color.Black
let size = (800, 800)
let win = createForm backgroundColor size title
let mutable planetCoordX = 0
let mutable planetCoordY = 0
let mutable tickCounter = 0
let mutable maxRuns = 0   // 0 to Int32

let drawPlanets() =
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightGray), 2.0F),planetCoordX+4,planetCoordY+4,2,2)) //Mercury
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Goldenrod), 2.0F),planetCoordX+7,planetCoordY+7,2,2)) //Venus
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.CornflowerBlue), 2.0F),planetCoordX+9,planetCoordY+9,2,2)) //Earf
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Maroon), 2.0F),planetCoordX+14,planetCoordY+14,2,2)) //Mars
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightCoral), 5.0F),planetCoordX+51,planetCoordY+51,5,5)) //Jupiter
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.DarkKhaki), 5.0F),planetCoordX+90,planetCoordY+90,5,5)) //Saturn
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.LightSkyBlue), 5.0F),planetCoordX+180,planetCoordY+180,3,3)) //Uranus
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.RoyalBlue), 5.0F),planetCoordX+300,planetCoordY+300,3,3)) //Neptune
    win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Tan), 2.0F),planetCoordX+320,planetCoordY+320,2,2)) //Pluto

let tbTime = new TextBox()
tbTime.Width <- 100
tbTime.Height <- 50
tbTime.Visible <- true
tbTime.Enabled <- true
tbTime.Location <- new Point (10, 10)
win.Controls.Add tbTime

let clicky (e : System.EventArgs) =
    maxRuns <- ((int)tbTime.Text * 10)
    timer.Enabled <- true
    butan.Enabled <- false
    tbTime.Enabled <- false

butan.Text <- "GO!"
butan.Height <- 50
butan.Width <- 50
butan.Location <- new Point (10, 50)
butan.ForeColor <- Color.White
butan.Click.Add clicky
win.Controls.Add butan


let updatePlanets (form : Form) timerEventMagic =
    planetCoordX <- planetCoordX + 1
    tickCounter <- tickCounter + 1
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



drawPlanets()

Application.Run win
