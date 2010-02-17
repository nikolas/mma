import FRP.Reactive

type BellMachine = Event () -> Event ()

doorBell :: BellMachine
doorBell = id

timer :: Event ()
timer = atTime 30
