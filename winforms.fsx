open System.Windows.Forms
open System.Drawing


let winHeight = 600
let winWidth = 600
let origo = (winHeight/2, winWidth/2)
let pos = 
  [[100;200];
  [100;300];
  [100;400];
  ]
(*
let updatePoints (form : Form) (coords : polyline byref) dtheta center showtime =
  let centeredCoords = translatePoints (scalePoint -1.0 center) coords
  let rotatedCoords = rotatePoints dtheta centeredCoords
  coords <- translatePoints center rotatedCoords
  form.Refresh ()
*)

let win = new Form ()
win.Text <- "title"
win.BackColor <- Color.White
win.ClientSize <- Size (winHeight, winWidth)



let drawPlanets() =
   win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Blue), 5.0F),400,400,5,5))  
   win.Paint.Add (fun e -> e.Graphics.DrawEllipse (new Pen ((Color.Red), 5.0F),300,300,5,5))  
let updatePlanets() = 
  win.Refresh()

// Add timer events
let timer = new Timer()
timer.Interval <- 1000
timer.Enabled <- true  
timer.Tick.Add (updatePlanets())



Application.Run win
