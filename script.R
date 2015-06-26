###############LOAD PACKAGES##########
require(rjson)
require(plyr)
require(dplyr)
require(lubridate)
require(streamgraph)

round_any <- function(x, accuracy, f = round) {
  UseMethod("round_any")
}

#' @export
round_any.numeric <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @export
round_any.POSIXct <- function(x, accuracy, f = round) {
  tz <- format(x[1], "%Z")
  xr <- round_any(as.numeric(x), accuracy, f)
  as.POSIXct(xr, origin="1970-01-01 00:00.00 UTC", tz=tz)
}

#######CREATES LIST OF JSON FOR EACH ARTISTS#############

folders<-list.files(path='artists/')
artist_json_list<-list()

for(folder in folders){print(folder)
  folder_files<-list.files(path=paste0('artists/',folder,'/'))
  print(folder_files)
  for(file in folder_files){
    path<-paste0('artists/',folder,'/',file)
    print(path)
    artist_json_list[[length(artist_json_list)+1]]<-fromJSON(file=path)
  }
}

#######CREATES LIST OF JSON FOR EACH PIECE OF ART#############

level1<-list.files(path='artworks/')
art_json_list<-list()

for(folder in level1){print(folder)
                       folder_folders<-list.files(path=paste0('artworks/',folder,'/'))
                      # print(folder_folders)
                       for(next_folder in folder_folders){
                         folder_files<-list.files(path=paste0('artworks/',folder,'/',next_folder,'/'))
                        # print(folder_files)
                         for(file in folder_files){
                           path<-paste0('artworks/',folder,'/',next_folder,'/',file)
                           #print(path)
                           art_json_list[[length(art_json_list)+1]]<-fromJSON(file=path)
                         }
                       }
}
                         
                         
########CREATE ARTIST TABLE BY TAKING USEFUL PIECES OF JSON##############

artist_to_data<-function(json_list){try({
string<-unlist(json_list)
vector<-c(string['fc'],
          string['id'],
          string['birth.place.placeName'],
          string['birthYear'],
          string['movements.name'],
          string['gender'],
          string['totalWorks'])
return(vector)
})
}

artist<-ldply(artist_json_list,artist_to_data)
names(artist)<-c('fc','id','birth.place.placeName','artist_birthYear','movement','gender','totalWorks')

########CREATE ART TABLE BY TAKING USEFUL PIECES OF JSON##############

art_to_data<-function(json_list){try({
  string<-unlist(json_list)
#  print(string)
  vector<-c(string['acno'],
            string['acquisitionYear'],
            string['contributors.fc'],
            string['contributors.id'],
            string['movements.name'],
            string['title'],
            string['dateRange.startYear'])
  return(vector)
})
}

art<-ldply(art_json_list,art_to_data)
names(art)<-c('acno','acquisitionYear','contributors.fc','contributors.id','movement','title','artwork_year')

#########MERGE ART AND ARTISTS AND MAKE SOME ALTERATIONS###########
combined<-left_join(art,artist,by=c('contributors.id'='id'))%>%
  mutate(estimated_movement=ifelse(is.na(movement.x),movement.y,movement.x),
         estimated_artwork_year=ifelse(is.na(artwork_year),artist_birthYear,artwork_year))%>%
  mutate(estimated_artwork_decade=ymd(as.Date(paste0(round_any(as.numeric(estimated_artwork_year),10,f=floor),'-01-01'))))

###### MAKE SOME SUMMARIES ETC FOR PLOTTING#############
detach("package:plyr")

#Get list of movements with >150 pieces of work
movements<-filter(combined,is.na(estimated_movement)==FALSE,estimated_movement!='Picturesque',estimated_artwork_decade>ymd('1900-01-01'),is.na(estimated_artwork_decade)==FALSE)%>%
  group_by(estimated_movement)%>%
  summarise(works=n_distinct(acno))%>%
  top_n(9,works)%>%
  select(estimated_movement)%>%
  .$estimated_movement

movements_stream_data<-filter(combined,estimated_movement %in% movements,estimated_artwork_decade>ymd('1900-01-01'))%>%
  group_by(estimated_movement,estimated_artwork_decade)%>%
  summarise(works=n_distinct(acno))%>%
  filter(is.na(estimated_artwork_decade)==FALSE)

streamgraph(movements_stream_data,"estimated_movement","works","estimated_artwork_decade")%>%
  sg_fill_brewer("Set3")%>%
  sg_axis_x(20, "estimated_artwork_decade", "%Y")%>%
  sg_legend(show=TRUE, label="Movement: ")

