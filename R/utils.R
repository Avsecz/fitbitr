## Convenient functions

##" Merge a list of data.tables by merge_by vector of variables
##"
##" @param dt_list A list of data.tables of data.frames.
##" @param merge_by Vector of variables to merge by.
##" @return Merged data.table.
mergelist <- function(dt_list,merge_by, all = all){
    Reduce(function(...) merge(...,by=merge_by, all = all),dt_list)
}
  
##" Colbind dt
cbindlist <- function(dt_list){
    Reduce(function(...) data.table(...),dt_list)
}


##" @param from_date Character date of form YYYY-MM-dd
##" @param to_date Character date of form YYYY-MM-dd
##" @return vector of all day
date_seq <- function(from_date, to_date){
  date_range <- as.character(seq(from = as.Date(from_date),
                                 to = as.Date(to_date),
                                 by="days"))
  return(date_range)
}


##" Clean character vector of '-' and ' '.
##"
##" Use with setnames(dt, clean_name(names(dt))).
clean_name <- function(name){
  ## replace space or -
  regex <- "-| "
  gsub(regex,"_", name)
}
