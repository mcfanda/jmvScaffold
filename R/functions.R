
say<-function(...) cat(crayon::blue(paste("\u2139 ", padstr(...),"\n")))

warn<-function(...) cat(crayon:red(paste("\u26A0 ",padstr(...),"\n")))


padstr<-function(...) {
 a<-paste(list(...), collapse=" ")
 b<-stringr::str_pad(paste(" ",a," "), 45, side="right", pad="-")
 stringr::str_pad(b, 50, side="left", pad="-")
}

