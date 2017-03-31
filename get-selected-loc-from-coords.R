## Get selected loc from coords

df <- tibble(
  "send.lat" = c(1,2,4,5,6),
  "send.lon" = c(1,2,4,5,6),
  "send.name" = c("target","other","Other","other","other"),
  "receive.lat" = c(1,4,1,1,5),
  "receive.lon" = c(6,9,3,1,10),
  "receive.name" = c("foo","foo","foo","target","foo")
)



df %>%
  filter({send.lat == 1 & send.lon == 1} |
           {receive.lat == 1 & receive.lon == 1})


df %>%
  mutate(selected.by.send = ifelse(send.lat == 1 & send.lon == 1, TRUE, FALSE)) %>%
  mutate(selected.by.receive = ifelse(receive.lat == 1 & receive.lon == 1, TRUE, FALSE)) %>%
  filter(selected.by.send == TRUE | selected.by.receive == TRUE) %>%
  mutate(select.loc.name = ifelse(selected.by.send == TRUE, send.name, receive.name)) %>%
  select(select.loc.name) %>%
  .[[1]]

%>%
  filter(selected.location == TRUE)


df %>%
  mutate(selected.location = ifelse({send.lat == 1 & send.lon == 1} | {receive.lat == 1 & receive.lon == 1} , TRUE, FALSE)) %>%
  filter(selected.location == TRUE) %>%
  rename(select.loc.name = send.name)

