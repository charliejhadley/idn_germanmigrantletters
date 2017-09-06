library(rfigshare)

update_fs_article <-
  function(fs.id,
           zip.name,
           files.to.ignore,
           publish = FALSE) {
    files_to_zip <- list.files()
    files_to_zip <- setdiff(files_to_zip, files.to.ignore)
    
    # my_deposit <- fs_details(fs.id)
    my_files <- data.frame(t(sapply(my_deposit$files, `[`)))
    
    print(my_files)
    
    # delete current files
    
    # lapply(unlist(my_files$id), function(id) {
    #   fs_delete(fs.id, id)
    # })
    
    
    
    # make new zip
    # zip(zip.name, files_to_zip)
    # fs_upload(fs.id, zip.name)
    
    if (publish == TRUE) {
      fs_make_public(fs.id)
    }
  }




update_fs_article(
  XXXX,
  ".zip",
  files.to.ignore = c(
    "rsconnect",
    ".httr-oauth"
  )
)
