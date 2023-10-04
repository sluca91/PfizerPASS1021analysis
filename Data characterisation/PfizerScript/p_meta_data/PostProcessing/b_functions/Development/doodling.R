x <- "10000"
x <- "string"
x <- "10.50111"

# x <- formatC(as.integer(x),digits = 0, big.mark=",")
x

y <-suppressWarnings(as.numeric(x)) 
is.na(y)
y

if(as.numeric(x) != round(as.numeric(x))){
  z=TRUE
}

if (!is.na(y) && is.numeric(y) ){ 
                  
                  if (y %% 1 == 0) {
                    x <- formatC(y, digits = 0, big.mark = ",", format= "f")
                  
                  } else if (y != round(y)){
                  
                    x <- formatC(y, digits = 2, decimal.mark = ".",big.mark = ",", format= "f")
                    }
                }
                


# else{}
    # }    
    # }

x




is.numeric(as.numeric(x))




as.numeric(x)


vec <- c("50", "200", "1,000"
         , "10", "1200", "2,100")  # Create example vector
vec 

as.numeric(vec)     


x = NA
if(is.na(x)) {x=T} else {if(x) {x}} 
x

survival::diabetic


ft <- flextable(survival::diabetic, col_keys = c("id", "laserage", "eye","trt","risk","time")) %>%
  mk_par(j = "laserage", value = as_paragraph(as_i(laser)," (" ,as_b(age),")")) %>% 
  color(j = "laserage", color = "#006699") %>% 
  set_header_labels(laserage = "Laser Age")
ft


?as_paragraph

?as_i #italic chunk
?as_b # bold chunk



as.logical(list(1))

list(1) == TRUE

if (list(1)) print("Passed test!")
if (as.logical(list(1))) print("Passed test!")

a = list(x = 1, y = 0)
as.logical(a)

if("true") "ok" # ok
if(-1) "ok" # ok


val = 1
class(val) = "test"
attr(a, "something") = 0

if(val) "ok" # ok
val


val = factor("TRUE", "FALSE")
as.numeric(val) # 1 (for TRUE), 2 (for FALSE)
