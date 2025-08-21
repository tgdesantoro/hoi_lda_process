
library(readr)
orangutannews <- read.csv2("D:/011_git/002_text_mining/topic_modelling/006_data-tmse_imo-news-2011-2024.csv")


View(orangutannews)
Encoding(orangutannews$content_clean)

names(orangutannews)
contentou <- orangutannews$content_clean
typeof(contentou)

library(dplyr)       #manipulasi data
library(stringr)     #manipulasi teks
library(tidytext)    #olahdata
library(magrittr)    #aktivasi pipe (fungsi dalam fungsi)
citation("magrittr")
library(stringi)     #manipulasi dan pengolahan string
library(purrr)      #deteksi kesalahan atau nilai NA
library(tm)          #text mining


#1st step tokenize unigram
unigram_ounews <- orangutannews %>%
  mutate(line_id = row_number()) %>%                                      #Buat ID baris agar urutan tetap terjaga
  unnest_tokens(word, content_clean, token = "words", to_lower = TRUE) %>%    #Tanpa pembersihan stop_words !!! token = "words"/1 words| token = "ngrams", n = 2,3, ., etc
  group_by(doc_id) %>%
  mutate(word_position = row_number()) %>%                                #Menyimpan posisi kata dalam dokumen
  ungroup()

#view position id word
head(unigram_ounews)
View(unigram_ounews)

#membersihkan kata yang tidak berarti
stopwords_id <- readLines("D:/011_git/001_raw-data/orangutan_news/ID-Stopwords-master/id.stopwords.02.01.2016.txt") 

# Menghapus stopwords dari barisan kata
unigram_ounews01 <- filter(unigram_ounews,!(word %in% stopwords_id))


#analisis tf-idf "melihat kepentingan kata"
tfidf_first_ou_content <- unigram_ounews01 %>%
  count(doc_id, word, sort = TRUE) %>%  # Menghitung TF (frekuensi kata per dokumen) no itu nomor artikel
  bind_tf_idf(word, doc_id, n)          # Menghitung TF-IDF

View(unigram_ounews01)
View(tfidf_first_ou_content)

# Hapus kata yang termasuk dalam daftar kata yang tidak diinginkan
unigram_ounews02 <- unigram_ounews01 %>%
  filter(!word %in% c("kalbar", "kalteng", "kaltim", "kalsel")) %>% 
  filter(!word %in% c("senin", "selasa", "rabu", "kamis", "jumat", "sabtu", "minggu")) %>% 
  filter(!word %in% c("ade", "abduh","aceh","affandi","apaapa","anyin","ardi","astri","afrika","aji","albertus","andika",
                      "boncel","dendi","fitria","muriansyah","suriani","taufik","samosir","agung","agustinus",
                      "sustyo","paulus","jamri","ariansyah","arifin","argitoe","sibarani",
                      "barata","bharata","aan","adi","adirahmanta","adisa","ariyadi",
                      "azmadi","chadidjah","hartono","hadi","hadiatul","heri","hermayadi","hernowo","idarno","iriyoni","iriono","iryono","irwiratno",
                      "iskandar","purnomo","suciadi","suryadi","supianor","sutiadi","yadi","agus","agungia","ahmad","aini","ali","aliansah","amalia",
                      "and","anthonius","antoro","anwar","ardhanianto","ari","arsyad","bambang",
                      "windy","gail","campbell","karmele","llano","martono","masriwiyono","muria", "noor","sadtata","sanchez","seno","sihantoro","siti",
                      "suryani","susilo","suyono","siregar","simon","silva","siponti","tri","trimarsito","trismanto","yuliana","zola",
                      "zoya","zulfiqri","yusuf","yudhi","yudi","yudas","zariansyah","yosafat","yohanes",
                      "yakobus","yansyah","yansen","yayan","yani","yanti","wiwied","wisnu","wiratno","wirendro",
                      "windaryati","widodo","widiantoro","wicaksono","wibawanto","wawan","utomo",
                      "utomo","utami","tjahyana","tito","heribertus","joss")) %>% 
  filter(!word %in% c("januari","februari","maret","april","mei","juni","juli","agustus","september","oktober","november","desember")) %>% 
  filter(!word %in% c("utara","barat","selatan","timur")) %>% 
  filter(!word %in% c("wita","wib")) %>%
  filter(!word %in% c("si","no","pas")) %>%
  filter(!word %in% c("perundang","undangan")) %>%
  filter(!word %in% c("nama","bernama","dinamai","namanya")) %>% 
  filter(!word %in% c("nol","satu","kesatu","dua","kedua","tiga","ketiga","empat","keempat",
                      "lima","kelima","enam","keenam","tujuh","ketujuh","delapan","kedelapan",
                      "sembilan","kesembilan","sepuluh","kesepuluh"))

View(unigram_ounews02)

# Menghapus "di" jika berada di awal kata (OPTIONAL)
unigram_ounews03 <- unigram_ounews02 %>%
  mutate(word = str_replace(word, "^di(?=[a-z])", "")) # "^di" = hapus "di" di awal kata, (?=[a-z]) memastikan hanya kata-kata valid yang berubah

# Menghapus "nya" jika berada di akhir kata
unigram_ounews04 <- unigram_ounews03 %>%
  mutate(word = str_replace(word, "nya$", ""))  # "nya$" = hapus "nya" hanya jika berada di akhir kata
View(unigram_ounews04)

# Mengganti kata yang sesuai dengan 'bksda, yiari, bosf'
unigram_ounews05 <- unigram_ounews04 %>%
  mutate(word = str_replace(word, "^(bksd|ksda|pushbksda)$", "bksda")) %>% 
  mutate(word = str_replace(word, "^(iar|iari)$", "yiari")) %>% 
  mutate(word = str_replace(word, "^(bos)$", "bosf"))

# Mengganti tokenisasai 'unigram' tidak baku menjadi baku, dan persamaan kata 
unigram_ounews06 <- unigram_ounews05 %>%
  mutate(word = trimws(word)) %>%  # Hapus spasi ekstra
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%  # Hapus tanda baca
  mutate(word = str_replace(word, "^(mentranslokasi|translokasikan|mentranslokasikan|translokasi|tranlokasi|dipindahkan|dipindah)$", "memindahkan")) %>% 
  mutate(word = str_replace(word, "^(menghimbau)$", "mengimbau")) %>% 
  mutate(word = str_replace(word, "^(ha|hektare)$", "hektar")) %>% 
  mutate(word = str_replace(word, "^(cm|sentimeter)$", "centimeter")) %>% 
  mutate(word = str_replace(word, "^(kawasan)$", "area")) %>%
  mutate(word = str_replace(word, "^(lantaran)$", "karena")) %>%
  mutate(word = str_replace(word, "^(habituasi)$", "adaptasi")) %>% 
  mutate(word = str_replace(word, "^(dikonversi)$", "berubah")) %>% 
  mutate(word = str_replace(word, "^(mengajari)$", "mengajarkan")) %>% 
  mutate(word = str_replace(word, "^(luka|terluka|lukaluka)$", "cedera")) %>% 
  mutate(word = str_replace(word, "^(laporkan|melapor)$", "melaporkan")) %>% 
  mutate(word = str_replace(word, "^(memonitor|monitoring|dipantau|pemantauan)$", "memantau")) %>% 
  mutate(word = str_replace(word, "^(tersisa)$", "sisa")) %>% 
  mutate(word = str_replace(word, "^(membopong)$", "membawa")) %>% 
  mutate(word = str_replace(word, "^(ekor|seekor)$", "individu")) %>% 
  mutate(word = str_replace(word, "^(melepasliar|meliarkan|lepasliarkan|lepas|dirilis)$", "melepasliarkan")) %>% 
  mutate(word = str_replace(word, "^(perjualbelikan|perdagangkan|memperjual|memperdagangkan|memperniagakan)$", "memperjualbelikan")) %>% 
  mutate(word = str_replace(word, "^(menaklukan)$", "menaklukkan")) %>% 
  mutate(word = str_replace(word, "^(mencarikan|mencariny|pencarian|cari)$", "mencari")) %>% 
  mutate(word = str_replace(word, "^(mencengkram)$", "mencengkeram")) %>% 
  mutate(word = str_replace(word, "^(mendokumentasikan|catat|tercatat|mencatat|mencatatkan)$", "mendokumentasi")) %>% 
  mutate(word = str_replace(word, "^(titip)$", "titipkan")) %>% 
  mutate(word = str_replace(word, "^(catatan)$", "dokumentasi")) %>% 
  mutate(word = str_replace(word, "^(berisiko|resiko)$", "risiko")) %>% 
  mutate(word = str_replace(word, "^(memasuki|masuk|memasukkan|masukkan|masukan)$", "masuk")) %>% 
  mutate(word = str_replace(word, "^(berladang)$", "berkebun")) %>% 
  mutate(word = str_replace(word, "^(kebunku)$", "kebun")) %>% 
  mutate(word = str_replace(word, "^(kebunkebun)$", "perkebunan")) %>% 
  mutate(word = str_replace(word, "^(terdeteksi)$", "terpantau")) %>% 
  mutate(word = str_replace(word, "^(anakanak|anakan)$", "anak")) %>% 
  mutate(word = str_replace(word, "^(mengakibatkan|sebabkan)$", "menyebabkan")) %>% 
  mutate(word = str_replace(word, "^(untung)$", "beruntung")) %>% 
  mutate(word = str_replace(word, "^(pembakaran)$", "membakar")) %>% 
  mutate(word = str_replace(word, "^(penemuan|temukan)$", "menemukan")) %>%
  mutate(word = str_replace(word, "^(engevakuasi)$", "evakuasi")) %>%
  mutate(word = str_replace(word, "^(memakan)$", "makan")) %>% 
  mutate(word = str_replace(word, "^(serahkan)$", "menyerahkan")) %>% 
  mutate(word = str_replace(word, "^(selamatkan)$", "menyelamatkan")) %>% 
  mutate(word = str_replace(word, "^(wargamei)$", "warga mei")) %>%
  mutate(word = str_replace(word, "^(hewan)$", "satwa")) %>%
  mutate(word = str_replace(word, "^(memelihara)$", "pelihara")) %>%
  mutate(word = str_replace(word, "^(undangundang)$", "peraturan")) %>%
  mutate(word = str_replace(word, "^(tewas)$", "mati")) %>%
  mutate(word = str_replace(word, "^(terfragmentasi)$", "fragmentasi")) %>%
  mutate(word = str_replace(word, "^(meningkatdari)$", "meningkat")) %>%
  mutate(word = str_replace(word, "^(berkelamin|berjenis)$", "kelamin")) %>%
  mutate(word = str_replace(word, "^(koordiantor)$", "koordinator")) %>%
  mutate(word = str_replace(word, "^(sosialiasi|sosialisasikan|bersosialisasi|mensosialisasikan|menyosialisasikan)$", "sosialisasi")) %>%
  mutate(word = str_replace(word, "^(pekan|sepekan)$", "")) %>%
  mutate(word = str_replace(word, "^(usia|berusia|seusia)$", "")) %>%
  mutate(word = str_replace(word, "^(bayibayi)$", "bayi")) %>%
  mutate(word = str_replace(word, "^(kronologis)$", "kronologi")) %>%
  mutate(word = str_replace(word, "^(siswasiswa|siswasiswi|siswi)$", "siswa")) %>%
  mutate(word = str_replace(word, "^(bergerakgerak|bergoyang|bergoyanggoyang)$", "bergerak")) %>%
  mutate(word = str_replace(word, "^(berhatihati)$", "waspada")) %>%
  mutate(word = str_replace(word, "^(berdasarkan)$", "")) %>%
  mutate(word = str_replace(word, "^(tribun)$", "")) %>%
  mutate(word = str_replace(word, "^(orangutafan|orangutans)$", "orangutan"))

unique(unigram_ounews06$word)
View(unigram_ounews06)

# Hapus unigram yang hanya berisi spasi atau kosong
unigram_ounews07 <- unigram_ounews06 %>%
  filter(str_squish(word) != "") %>% 
  filter(.,!(word %in% stopwords_id))

View(unigram_ounews07)

#analisis tf-idf "melihat kepentingan kata"
tfidf_first_ou_content2 <- unigram_ounews07 %>%
  count(doc_id, word, sort = TRUE) %>%  # Menghitung TF (frekuensi kata per dokumen) no itu nomor artikel
  bind_tf_idf(word, doc_id, n)          # Menghitung TF-IDF

View(tfidf_first_ou_content2)


# **Mengembalikan ke Teks Asli Sesuai Urutan Awal**
content_restored02 <- unigram_ounews07 %>%
  arrange(doc_id, word_position) %>%  # Mengurutkan kembali berdasarkan posisi kata
  group_by(doc_id) %>%
  summarise(content_restored = str_c(word, collapse = " ")) %>%
  ungroup()

# Gabungkan kembali hasil tokenisasi ke database awal berdasarkan doc_id
orangutannews01 <- orangutannews %>%
  left_join(content_restored02, by = "doc_id")

View(orangutannews01)

######################compact label instansi########################################################################

# Menghapus kepanjangan instansi
orangutannews02 <- orangutannews01 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "yiari"), 
                                   str_replace(content_restored, "international animal rescue|inisiasi alam rehabilitasi", ""), 
                                   content_restored))

orangutannews03 <- orangutannews02 %>%
  mutate(content_restored = if_else(
    str_detect(content_restored, "bksda"),  # Cek apakah ada kata "bksda"
    str_replace_all(content_restored, 
                    "badan konservasi sumber daya alam|balai konservasi sumber daya alam|
                    konservasi sumber daya alam|skw|seksi konservasi wilayah", 
                    ""),  # Hapus frasa yang sesuai
    content_restored  # Jika tidak ada "bksda", tetap pakai teks asli
  ))

orangutannews04 <- orangutannews03 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "tn"), 
                                   str_replace(content_restored, "taman nasional", ""), 
                                   content_restored))

orangutannews05 <- orangutannews04 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "bosf"), 
                                   str_replace(content_restored, "borneo orangutan survival foundation", ""), 
                                   content_restored))

orangutannews06 <- orangutannews05 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "cop"), 
                                   str_replace(content_restored, "centre orangutan protection|centre for orangutan protection", ""), 
                                   content_restored))

orangutannews07 <- orangutannews06 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "ofi"), 
                                   str_replace(content_restored, "orangutan foundation international", ""), 
                                   content_restored))

orangutannews08 <- orangutannews07 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "soc"), 
                                   str_replace(content_restored, "sintang orangutan center", ""), 
                                   content_restored))

orangutannews09 <- orangutannews08 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "yp"), 
                                   str_replace(content_restored, "yayasan palung", ""), 
                                   content_restored))

orangutannews10 <- orangutannews09 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "fnpf"), 
                                   str_replace(content_restored, "friends of national park foundation", ""), 
                                   content_restored))

orangutannews11 <- orangutannews10 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "skw"), 
                                   str_replace(content_restored, "seksi konservasi wilayah", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "wru"), 
                                   str_replace(content_restored, "wildlife rescue unit", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "pt"), 
                                   str_replace(content_restored, "perusahaan", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "wssl"), 
                                   str_replace(content_restored, "wana sawit subur lestari", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "bga"), 
                                   str_replace(content_restored, "bumitama gunajaya agro", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "orangutan"), 
                                   str_replace(content_restored, "pongo pygmaeus", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "peraturan|perundang|undangan|perundangan"), 
                                   str_replace(content_restored, "pasal", ""), 
                                   content_restored))

orangutannews11 <- orangutannews11 %>%
  mutate(content_restored = ifelse(str_detect(content_restored, "penyelamatan"), 
                                   str_replace(content_restored, "rescue", ""), 
                                   content_restored))


View(orangutannews11)


##################menghilangkan lokasi administrasi########################################################3
list.files("D:/011_git/002_text_mining/ID-regional-master/")
adm_kalimantan <- read.csv2("D:/011_git/002_text_mining/ID-regional-master/reg_kalimantan.csv")
head(adm_kalimantan)

#Simpan daftar nama kabupaten dari reg_kalimantan sebagai vektor
nama_kabupaten <- adm_kalimantan$nama_kabupaten_big %>%
 paste(., collapse = "|") #Gabungkan nama kabupaten menjadi pola regex untuk pencarian

orangutannews12 <- orangutannews11 %>%
  mutate(content_restored = str_replace_all(content_restored, nama_kabupaten, ""))

#Simpan daftar nama kecamatan dari reg_kalimantan sebagai vektor
nama_kecamatan <- adm_kalimantan$nama_kecamatan_big %>%
  paste(., collapse = "|") #Gabungkan nama kecamatan menjadi pola regex untuk pencarian

orangutannews13 <- orangutannews12 %>%
  mutate(content_restored = str_replace_all(content_restored, nama_kecamatan, ""))

#Simpan daftar nama desa dari reg_kalimantan sebagai vektor
nama_desa <- adm_kalimantan$nama_desa_big %>%
  paste(., collapse = "|") #Gabungkan nama kecamatan menjadi pola regex untuk pencarian

orangutannews14 <- orangutannews13 %>%
  mutate(content_restored = str_replace_all(content_restored, nama_desa, ""))

orangutannews15 <- orangutannews14 %>%
  mutate(content_restored = str_replace_all(content_restored, "kabupaten pontianak", "kabupaten mempawah")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "kotim", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "kutim", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "kutai", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kutai timur", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "sangatta", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "tenggarong", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "sampit", "")) %>% 
  mutate(content_restored = str_replace_all(content_restored, "kabupaten", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kotawaringin timur", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kotawaringin barat", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kotawaringin", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "tana paser", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kecamatan siantan", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kecamatan mentawa ketapang", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kecamatan komam", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "pangkalan bun|pangkalanbun", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "ketapang", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "muara teweh", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "sintang", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "palangka raya", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "pontianak", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "sanggau", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "singkawang", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "singapura", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kayong agro lestari", "kal")) %>%
  mutate(content_restored = str_replace_all(content_restored, "wwfindonesia", "wwf")) %>%
  mutate(content_restored = str_replace_all(content_restored, "yogyakarta", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kayong", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "mempawah", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "seponti", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "sukadana", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kapuas", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "kecamatan", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "areaarea", "area")) %>%
  mutate(content_restored = str_replace_all(content_restored, "balai", "")) %>%
  mutate(content_restored = str_replace_all(content_restored, "konservasi sumber daya alam|skw|wru", "bksda")) %>%
  mutate(content_restored = str_replace_all(content_restored, "yayasan inisiasi alam rehabilitasi", "yiari")) %>%
  mutate(content_restored = str_replace_all(content_restored, "orangutan foundation indonesia", "ofi")) %>%
  mutate(content_restored = str_replace_all(content_restored, "taman nasional", "tn")) %>%
  mutate(content_restored = str_replace_all(content_restored, "perusahaan", "pt")) %>%
  mutate(content_restored = str_replace_all(content_restored, "pasal", "peraturan")) %>%
  mutate(content_restored = gsub("\\s+", " ", content_restored))  # Ubah spasi lebih dari satu jadi satu

orangutannews16 <- orangutannews15 %>%
  mutate(content_restored = str_replace_all(content_restored, nama_kabupaten, ""))

View(orangutannews16)
write.csv2(orangutannews16,"D:/011_git/002_text_mining/topic_modelling/orangutannews_lda-process-rv.csv")
library(readr)
ou_17topik <- read.csv2("D:/011_git/002_text_mining/topic_modelling/orangutannews_lda-process-rv.csv")
names(ou_17topik)


#1st step tokenize unigram
unigram_ounews08 <- ou_17topik %>%
  mutate(line_id = row_number()) %>%                                      #Buat ID baris agar urutan tetap terjaga
  unnest_tokens(word, content_restored, token = "words", to_lower = TRUE) %>%    #Tanpa pembersihan stop_words !!! token = "words"/1 words| token = "ngrams", n = 2,3, ., etc
  group_by(doc_id) %>%
  mutate(word_position = row_number()) %>%                                #Menyimpan posisi kata dalam dokumen
  ungroup()

View(unigram_ounews08)

# **Mengembalikan ke Teks Asli Sesuai Urutan Awal**
content_news_clear_all01 <- unigram_ounews08 %>%
  arrange(doc_id, word_position) %>%  # Mengurutkan kembali berdasarkan posisi kata
  group_by(doc_id) %>%
  summarise(content_restored = str_c(word, collapse = " ")) %>%
  ungroup()

View(content_news_clear_all01)

# Gabungkan kembali hasil tokenisasi ke database awal berdasarkan doc_id
orangutannews_id <- orangutannews %>%
  left_join(content_news_clear_all01, by = "doc_id")

head(orangutannews_id)
colnames(orangutannews_id)

#word count
word_count <- str_count(orangutannews_id$content_restored, "\\S+")  # Hitung jumlah kata
word_count
# Hitung statistik
word_stats <- tibble(
  total_kata = sum(word_count),     # Total jumlah kata
  rata_rata  = mean(word_count),    # Rata-rata jumlah kata per baris
  minimal    = min(word_count),     # Jumlah kata minimal dalam satu baris
  maksimal   = max(word_count)      # Jumlah kata maksimal dalam satu baris
)
word_stats

###unigramterakhir######
#step tokenize unigram
unigram_contentou01 <- orangutannews_id %>%
  mutate(line_id = row_number()) %>%                                      #Buat ID baris agar urutan tetap terjaga
  unnest_tokens(word, content_restored, token = "words", to_lower = TRUE) %>%    #Tanpa pembersihan stop_words !!! token = "words"/1 words| token = "ngrams", n = 2,3, ., etc
  group_by(doc_id) %>%
  mutate(word_position = row_number()) %>%                                #Menyimpan posisi kata dalam dokumen
  ungroup()

unique_word_count <- unigram_contentou01 %>%
  distinct(word) %>%  # Ambil hanya kata unik
  nrow()  # Hitung jumlahnya

print(unique_word_count)
View(unigram_contentou01)

# Hitung frekuensi kata per dokumen
ouword_counts01 <- unigram_contentou01 %>%
  count(doc_id, word, sort = TRUE)

# Konversi ke format Document-Term Matrix (DTM)
dtm_unigram01 <- ouword_counts01 %>%
  cast_dtm(doc_id, word, n)

# Lihat ringkasan DTM
dtm_unigram01

############################unigram tokenize-LDA model using gibbs#############################################
install.packages("textmineR")
library(textmineR)
citation("textmineR")
library(topicmodels)
citation("topicmodels")
library(Matrix)
library(ggplot2)
library(slam)
citation ("slam")
library(ldatuning)
citation("ldatuning")
library(tidytext)
install.packages("tibble")  # Jika belum terinstal
library(tibble)
library(dplyr)
library(lsa)
library(tm)        # Untuk DocumentTermMatrix
library(tidyr)     # Untuk manipulasi data
library(scales)
library(textmineR)

########menjalankan LDA Model-gibbs method##############

#deteksi pembuatan model dari topik ke-n hingga ke-n dengan melihat nilai evaluasi

# Evaluasi dengan Perplexity untuk k = 3 sampai k = 20
perplexity_results01 <- tibble()

# Loop untuk berbagai jumlah topik (k)
for (k in 3:20) {
  cat("Running LDA for k =", k, "\n")  # Debugging output
  
  lda_modelgibbs <- LDA(dtm_unigram01, k = k, method = "Gibbs",
                        control = list(seed = 1234,  
                                       iter = 2000,  
                                       burnin = 500,  
                                       thin = 10))    
                                       #alpha = 50/k,  # Alpha default: 50/k  =>OPSIONAL
                                       #delta = 0.1))  # Delta untuk menghindari overfitting =>OPSIONAL
  
  # Menghitung perplexity
  perplexity_value <- perplexity(lda_modelgibbs, dtm_unigram01)
  
  # Simpan hasil
  perplexity_results01 <- bind_rows(perplexity_results01, tibble(k = k, perplexity = perplexity_value))
}

# Normalisasi perplexity ke rentang 0 - 1
min_ppl <- min(perplexity_results01$perplexity)
max_ppl <- max(perplexity_results01$perplexity)

perplexity_results_normalized01 <- perplexity_results01 %>%
  mutate(perplexity_normalized = (perplexity - min_ppl) / (max_ppl - min_ppl))


# menghitung coherence score untuk k-n sampai k-n

# Memuat paket yang diperlukan#
# Misalkan dtm_unigram01 adalah objek DocumentTermMatrix yang sudah ada
# Konversi DocumentTermMatrix (DTM) menjadi sparse matrix (dgCMatrix)
dim(dtm_unigram01)

# Konversi DTM ke sparse matrix
dtm_unigram01_sparse <- as.matrix(dtm_unigram01)  # Ubah ke matrix biasa dulu
dtm_unigram01_sparse <- Matrix(dtm_unigram01_sparse, sparse = TRUE)  # Konversi ke dgCMatrix
dim(dtm_unigram01_sparse)

row_sums <- rowSums(as.matrix(dtm_unigram01_sparse))
sum(row_sums == 0)  # Hitung jumlah dokumen kosong

# Dataframe untuk menyimpan hasil
coherence_results <- tibble()

# Loop untuk berbagai jumlah topik
for (k in 3:20) {
  cat("Running LDA for k =", k, "\n")  
  
  # Menjalankan LDA dengan metode Gibbs
  lda_modelgibbs <- LDA(dtm_unigram01_sparse, k = k, method = "Gibbs",
                        control = list(seed = 1234, 
                                       iter = 2000, 
                                       burnin = 500, 
                                       thin = 10))
                                       #alpha = 50/k, => opsional
                                       #delta = 0.1)) =>opsional
  
  # **1. Ekstrak distribusi kata-topik (phi)**
  phi <- as.matrix(lda_modelgibbs@beta)  # Mengambil beta dari model LDA
  
  # **2. Pastikan kolom phi sesuai dengan DTM**
  colnames(phi) <- colnames(dtm_unigram01_sparse)  # Sesuaikan nama kolom
  
  # **3. Ekstrak topik dari model**
  topics_gibbs <- tidy(lda_modelgibbs, matrix = "beta")
  
  # **4. Ambil 10 kata utama per topik**
  top_terms <- topics_gibbs %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 10) %>% 
    ungroup()
  
  # **5. Konversi ke list untuk coherence**
  top_terms_list <- split(top_terms$term, top_terms$topic)
  
  # **6. Hitung coherence score**
  coherence_score <- CalcProbCoherence(phi = phi, dtm = dtm_unigram01_sparse, M = 10)
  
  # **7. Simpan hasil coherence**
  coherence_results <- bind_rows(coherence_results, tibble(k = k, coherence = mean(coherence_score)))
}

# Gabungkan hasil perplexity dan coherence
evaluation_topic_results <- left_join(perplexity_results01, coherence_results, by = "k")
View(evaluation_topic_results)
write.csv2(evaluation_topic_results,"D:/011_git/002_text_mining/topic_modelling/evaluation_topic_results-rv08.csv")


# Menjalankan LDA-only k ke-n
lda_gibbs11 <- LDA(dtm_unigram01, k = 11, method = "Gibbs", 
                  control = list(seed = 1234,  # Reproducibility
                                 iter = 2000,  # Jumlah iterasi
                                 burnin = 500, # Iterasi awal yang dibuang
                                 thin = 10,   # Interval pengambilan sampel
                                 alpha = 50/20, # Alpha default: 50/k   
                                 delta = 0.2)) # Beta default: 0.1

View(lda_gibbs11)

# Ekstrak topik dari model
topics_gibbs11 <- tidy(lda_gibbs11, matrix = "beta")


# Ambil 10 kata utama per topik
top_terms_gibbs11 <- topics_gibbs11 %>% group_by(topic) %>% slice_max(beta, n = 10) %>% ungroup()

# Mengurutkan dan mengambil semua kata berdasarkan beta (probabilitas)
all_terms_gibbs11 <- topics_gibbs11 %>% group_by(topic) %>% arrange(desc(beta)) %>% ungroup()

View(all_terms_gibbs11)
write.csv2(all_terms_gibbs11,"D:/011_git/002_text_mining/topic_modelling/all_terms_gibbs-011-02.csv")


# Mengambil distribusi topik dari hasil LDA Gibbs
topic_distribution <- as.data.frame(lda_gibbs11@gamma)

# Menambahkan kolom ID dokumen
topic_distribution$doc_id <- 1:nrow(topic_distribution)

# Melihat ringkasan distribusi probabilitas topik
summary(topic_distribution)
View(topic_distribution)

#melihat 10 kata teratas tiap cluster pada sampel pembagian custer
View(top_terms_gibbs6)
str(top_terms_gibbs6)


# Visualisasi kata per topik
library(ggplot2)
library(dplyr)
library(tidytext)  # untuk reorder_within() dan scale_y_reordered()

# Tentukan nilai beta maksimum dari seluruh data sebagai acuan sumbu X
max_beta <- max(top_terms_gibbs11$beta)

top_terms_gibbs11 %>%
  # Urutkan kata berdasarkan beta di dalam tiap topik
  group_by(topic) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(word = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = word)) +
  geom_col(fill = "grey60", show.legend = FALSE) +  # warna abu-abu
  facet_wrap(~ topic, scales = "free_y") +  # hanya y yang bebas, x konsisten
  scale_y_reordered() +
  scale_x_continuous(
    limits = c(0, max_beta),  # semua grafik pakai skala x yang sama
    breaks = seq(0, max_beta, length.out = 5),  # interval merata
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "", 
    x = "Term Probability", 
    y = "Term"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),  # Ukuran judul topik
    axis.title.x = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 11)
  )

###cosine similarity topic###
# Tambahan paket yang mungkin belum kamu load
library(tidyr)
library(dplyr)
library(lsa)         # Untuk cosine similarity
install.packages("pheatmap")
library(pheatmap)    # Untuk heatmap


# ---- 1. Ubah data beta menjadi matriks topik-kata (rows = topics, columns = terms) ----
topic_term_matrix <- topics_gibbs11 %>%
  tidyr::pivot_wider(names_from = term, values_from = beta, values_fill = 0) %>%
  arrange(topic) %>%
  column_to_rownames(var = "topic") %>%
  as.matrix()

# ---- 2. Hitung cosine similarity antar topik (baris) ----
cosine_sim_matrix <- cosine(t(topic_term_matrix))  # transpose supaya topik jadi vektor
View(cosine_sim_matrix)
write.csv2(cosine_sim_matrix,"D:/011_git/002_text_mining/topic_modelling/cosine_sim_matrix-011.csv")

# ---- 3. Buat heatmap seperti contoh ----
pheatmap(
  cosine_sim_matrix,
  color = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
  main = "Cosine Similarity",
  display_numbers = FALSE,
  legend = TRUE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  
  # Atur rotasi teks sumbu X dan Y
  angle_col = 0,            # label X tidak miring
  fontsize = 11,
  fontsize_row = 11,
  fontsize_col = 11,
  border_color = NA,         # HILANGKAN garis antar sel (hilangkan grid gelap
  )

library(tidyr)
library(dplyr)
library(pheatmap)
citation("pheatmap")

# Ambil Top-N terms per topik dari objek topics_gibbs11
top_n <- 10
top_terms_jaccard <- topics_gibbs11 %>%
  group_by(topic) %>%
  slice_max(beta, n = top_n) %>%
  summarise(terms = list(term)) %>%
  arrange(topic)

# Buat matriks kosong untuk simpan Jaccard
n_topics <- nrow(top_terms_jaccard)
jaccard_matrix <- matrix(0, nrow = n_topics, ncol = n_topics)
rownames(jaccard_matrix) <- paste0( 1:n_topics)
colnames(jaccard_matrix) <- paste0( 1:n_topics)

# Hitung Jaccard similarity
for (i in 1:n_topics) {
  for (j in 1:n_topics) {
    terms_i <- top_terms_jaccard$terms[[i]]
    terms_j <- top_terms_jaccard$terms[[j]]
    
    intersect_len <- length(intersect(terms_i, terms_j))
    union_len <- length(union(terms_i, terms_j))
    
    jaccard_matrix[i, j] <- intersect_len / union_len
  }
}

# View dan Simpan
View(jaccard_matrix)
write.csv2(jaccard_matrix, "D:/011_git/002_text_mining/topic_modelling/jaccard_sim_matrix-011.csv")

# Buat heatmap
pheatmap(
  jaccard_matrix,
  color = colorRampPalette(c("white", "grey", "black"))(100),
  main = paste("Jaccard Similarity"),
  display_numbers = FALSE,
  legend = TRUE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  angle_col = 0,
  fontsize = 11,
  fontsize_row = 11,
  fontsize_col = 11,
  border_color = NA
)









# Ubah ke data frame panjang (long format), hilangkan diagonal (self-similarity)
cosine_df <- as.data.frame(as.table(cosine_sim_matrix)) %>%
  rename(Topic1 = Var1, Topic2 = Var2, similarity = Freq) %>%
  filter(Topic1 != Topic2)  # buang diagonal

# Simpan pasangan topik yang paling mirip
most_similar_topics <- cosine_df %>%
  arrange(desc(similarity)) %>%
  slice(1:10)

print(most_similar_topics)


###membuat intertopic distance map#####
# Ekstrak distribusi topik per dokumen dari model LDA
doc_topics <- tidy(lda_gibbs11, matrix = "gamma")

# Hitung rata-rata distribusi setiap topik di seluruh dokumen
topic_distributions <- doc_topics %>%
  group_by(topic) %>%
  summarise(avg_gamma = mean(gamma)) %>%
  arrange(desc(avg_gamma))

# Lakukan Multidimensional Scaling (MDS) untuk merepresentasikan topik dalam ruang 2D
topic_distance_matrix <- dist(as.matrix(topic_distributions$avg_gamma))  # Ubah ke bentuk matriks
mds_result <- cmdscale(topic_distance_matrix, k = 2)  # MDS dengan 2 dimensi

# Konversi hasil MDS ke dalam data frame untuk visualisasi
mds_df <- data.frame(
  topic = topic_distributions$topic,
  x = mds_result[, 2],
  y = mds_result[, 1],
  size = topic_distributions$avg_gamma  # Ukuran lingkaran berdasarkan rata-rata gamma
)

# Plot menggunakan ggplot2
ggplot(mds_df, aes(x = x, y = y, size = size)) +
  geom_point(aes(fill = factor(topic)), shape = 21, color = "black", alpha = 0.6) +
  geom_text(aes(label = topic), vjust = 0, hjust = 0.5, size = 5, fontface = "bold") +
  scale_size(range = c(5, 30), labels = percent_format(accuracy = 1)) +  
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Intertopic Distance Map (MDS Scaling)",
    x = "PC2",
    y = "PC1"
  )



write.csv2(top_terms_gibbs23,"D:/011_git/002_text_mining/topic_modelling/top_terms_gibbs23.csv") #saving top ten terms per topic
write.csv2(all_terms_gibbs17,"D:/011_git/002_text_mining/topic_modelling/all_terms_gibbs15.csv") #saving all terms per topic


########################identifikasi topik tiap artikel############################################################

# Ekstrak distribusi probabilitas topik terhadap article
doc_topics_11gibbs <- tidy(lda_gibbs11, matrix = "gamma")
doc_topics_11gibbs
write.csv2(doc_topics_11gibbs,"D:/011_git/002_text_mining/topic_modelling/doc_gamma_topics11.csv")

# Lihat distribusi topik dari beberapa dokumen
doc_topics_11gibbs %>% head()
View(doc_topics_11gibbs)

hist(doc_topics_11gibbs$gamma)

#threshold berdasarkan distribusi gamma
threshold_dynamic <- mean(doc_topics_11gibbs$gamma) + sd(doc_topics_11gibbs$gamma)

# Hitung threshold berdasarkan 75th Percentile
threshold_percentile <- quantile(doc_topics_11gibbs$gamma, 0.75)

# Hitung threshold default berdasarkan jumlah topik (1/K)
K <- 11 # Sesuaikan dengan jumlah topik
threshold_default <- 1 / K

hist(doc_topics_11gibbs$gamma, 
     breaks = 30, 
     main = "Gamma Probability Distribution", 
     xlab = "Gamma Probability",  # Rename sumbu X
     ylab = "Frequency Article ",   # Rename sumbu Y
     col = "gray", 
     cex.axis = 1.1,   # Ukuran teks sumbu
     cex.lab = 1.2,    # Ukuran teks label sumbu
     cex.main = 1.5)   # Ukuran teks judul

# Menambahkan threshold dengan warna berbeda
abline(v = threshold_dynamic, col = "red", lwd = 1, lty = 2)   # Threshold dynamic mean +sd
abline(v = threshold_percentile, col = "blue", lwd = 1, lty = 2) # Threshold percentile 75%
abline(v = threshold_default, col = "green", lwd = 1, lty = 2)  # Threshold default (1/K)

# menentukan berdasarkan topik yang signifikan (distribusi gamma) 
topik_terdeteksi_11gibbs <- doc_topics_11gibbs %>%
  filter(gamma >= threshold_default) %>%
  arrange(document, desc(gamma))

hist(topik_terdeteksi_11gibbs$gamma)
View(topik_terdeteksi_11gibbs)
write.csv2(topik_terdeteksi_11gibbs,"D:/011_git/002_text_mining/topic_modelling/topik_selective_11gibbs.csv")

# Hitung jumlah kemunculan tiap topik (after filter threshold)
topic_counts_threshold <- topik_terdeteksi_11gibbs %>%
  count(topic)


# Visualisasikan
ggplot(topic_counts_threshold, aes(x = factor(topic), y = n)) +
  geom_col(fill = "grey60") +
  labs(
    title = "Number of Article per Topic (filter threshold)",
    x = "Topic",
    y = "Number of Article"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

head(topik_terdeteksi_16gibbs)
names(orangutannews16)

#change name of coloumn term into word on topic_model1
topik_terdeteksi_11gibbs <- topik_terdeteksi_11gibbs %>%
  rename(doc_id = document)


topik_terdeteksi_11gibbs <- topik_terdeteksi_11gibbs %>%
  mutate(doc_id = as.integer(doc_id))
colnames(topik_terdeteksi_11gibbs)


# Menggabungkan hasil filtering topik ke raw data berdasarkan doc_id
orangutannews11_topik <- orangutannews16 %>%
  left_join(topik_terdeteksi_11gibbs, by = "doc_id")

unique(orangutannews11_topik$date_publish01)
sum(is.na(orangutannews11_topik$date_publish01))

View(orangutannews11_topik)
write.csv2(orangutannews11_topik,"D:/011_git/002_text_mining/topic_modelling/orangutan-topic11_threshold.csv")

# Konversi kolom date_publish menjadi format Date
orangutannews11_topik <- orangutannews11_topik %>%
  mutate(date_publish01 = dmy(date_publish01)) # Format dari dd/mm/yyyy ke Date

View(orangutannews11_topik)
orangutannews11_topik <- orangutannews11_topik %>%
  select(-themes, -name_topic)

# Tambahkan kolom kategori berdasarkan nomor topik
orangutannews11_topik <- orangutannews11_topik %>%
  mutate(themes = case_when(
    topic %in% c(1, 2, 4, 5, 6) ~ "Interaction types",
    topic == 3 ~ "Landscape",
    topic %in% c(7, 9, 10, 11) ~ "Mitigation and handling",
    topic == 8 ~ "Interaction impact",
    TRUE ~ "Other"
  ))
View(orangutannews11_topik)

# Buat deskripsi manual tiap topik
topic_descriptions <- tibble(
  topic = 1:11,
  name_topic = c(
    "Orangutan sighting on transportation area",
    "Orangutan sighting in plantation",
    "Oil palm plantation existing",
    "Orangutan captive",
    "Orangutan raiding crop",
    "Orangutan hunting",
    "Translocation orangutan",
    "Orangutan injured Condition",
    "Handover orangutan",
    "Wildlife protected regulation",
    "Population and Habitat Orangutan"
  ))

# Gabungkan deskripsi topik ke dalam data LDA
orangutannews11_topik <- orangutannews11_topik %>%
  left_join(topic_descriptions, by = "topic")
View(orangutannews11_topik)
write.csv2(orangutannews11_topik,"D:/011_git/002_text_mining/topic_modelling/definition-topic-11_threshold.csv")

# Filter data berdasarkan tahun 2011 hingga 2016
data_filtered01 <- orangutannews11_topik %>%
  filter(year(date_publish01) >= 2011 & year(date_publish01) <= 2024)

# Hitung jumlah topik per tahun
topik_per_tahun <- data_filtered01 %>%
  count(year = year(date_publish01), name_topic)

# Buat salinan name_topic dengan pemisahan 2 kata pertama lalu ganti jadi 2 baris
topik_per_tahun <- topik_per_tahun %>%
  mutate(name_topic_wrapped = str_replace(name_topic, 
                                          "^((\\S+\\s+){2})", "\\1\n"))


topik_per_tahun$name_topic <- as.factor(topik_per_tahun$name_topic)

library(ggplot2)
library(dplyr)
library(stringr)

# Tambah kolom themes ke topik_per_tahun
topik_per_tahun01 <- data_filtered01 %>%
  count(year = year(date_publish01), topic) %>%
  left_join(topic_descriptions, by = "topic") %>%
  left_join(
    orangutannews11_topik %>% select(topic, themes) %>% distinct(),
    by = "topic"
  ) %>%
  mutate(
    name_topic_wrapped = str_replace(name_topic, "^((\\S+\\s+){2})", "\\1\n")
  )
View(topik_per_tahun01)

# Buat lookup unik dari topic ke name_topic_wrapped
levels_lookup <- topik_per_tahun01 %>%
  select(topic, name_topic_wrapped) %>%
  distinct() %>%
  arrange(topic) %>%
  pull(name_topic_wrapped)

topik_per_tahun02 <- topik_per_tahun01 %>%
  arrange(factor(themes, levels = c(
    "Mitigation and handling",
    "Interactions types",
    "Interactions impact",
    "Landscape",
    "other"
  )), topic) %>%
  mutate(
    name_topic_wrapped = factor(name_topic_wrapped, levels = unique(name_topic_wrapped)),
    themes = factor(themes)
  )


# Ambil posisi garis pemisah antar themes
topic_levels_df <- topik_per_tahun02 %>%
  distinct(name_topic_wrapped, themes) %>%
  arrange(factor(themes, levels = c(
    "Mitigation and handling",
    "Interactions types",
    "Interactions impact",
    "Landscape",
    "other"
  ))) %>%
  mutate(pos_y = as.numeric(factor(name_topic_wrapped, levels = rev(unique(name_topic_wrapped)))))

View(topic_levels_df)

breaks_pos_manual <- tibble(
  yintercept = c(4.5, 5.5, 10.5),  # Panjang = 3
  themes = c(
    "Mitigation and handling",
    "Interactions types",
    "Interactions impact"
  )
)

View(breaks_pos_manual)

theme_label_pos <- topik_per_tahun02 %>%
  distinct(name_topic_wrapped, themes) %>%
  group_by(themes) %>%
  summarise(pos = mean(as.numeric(name_topic_wrapped)), .groups = "drop") %>%
  mutate(
    label = case_when(
      str_count(themes, "\\w+") > 1 ~ str_replace(themes, "^((\\S+\\s+){1})", "\\1\n"),
      TRUE ~ themes
    )
  )

View(theme_label_pos)

# Plot utama
ggplot(topik_per_tahun02, aes(x = factor(year), y = name_topic_wrapped, fill = n)) +
  geom_tile(color = "white") +
  
  # Garis horizontal pemisah tema
  geom_hline(data = breaks_pos_manual, aes(yintercept = yintercept), color = "black", linewidth = 0.5) +
  
  # Formating: Tambah label kategori di kanan (themes)
  geom_text(data = theme_label_pos, 
            aes(x = max(as.numeric(as.factor(topik_per_tahun02$year))) + 0.6, 
                y = pos, label = label), 
            inherit.aes = FALSE,
            hjust = 0, fontface = "bold", color = "black", size = 4.5) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    values = scales::rescale(c(0, 2, 4, 6, 8, 10)),
    na.value = "grey90",
    limits = range(topik_per_tahun02$n, na.rm = TRUE)
  ) +
  labs(
    x = "Year",
    y = "Topic",
    fill = "Quantity",
    title = "Heatmap Latent Topic Distribution"
  ) +
  coord_cartesian(clip = "off") +  # supaya label di luar plot terlihat
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(10, 100, 10, 10)  # beri ruang di kanan
  )






install.packages("forcats")
library(forcats)

# Membuat heatmap dengan bentuk persegi seperti pixel
# Menambahkan kolom grup berdasarkan 4 bagian dokumen
topik_terdeteksi_11gibbs <- topik_terdeteksi_11gibbs %>%
  mutate(group = ntile(as.numeric(doc_id), 2))  # Membagi dokumen menjadi 2 bagian

topik_terdeteksi_11gibbss <- topik_terdeteksi_11gibbs %>%
  mutate(doc_id = sprintf("%03d", as.numeric(doc_id)))  # Pastikan angka menjadi 3 digit


# Pastikan data diurutkan terlebih dahulu
topik_terdeteksi_11gibbs <- topik_terdeteksi_11gibbs %>%
  arrange(doc_id)  # Mengurutkan dokumen dari kecil ke besar

#Konversi document ke karakter lalu faktor untuk menghindari error
topik_terdeteksi_11gibbs <- topik_terdeteksi_11gibbs %>%
  mutate(
    doc_id = factor(sprintf("%03d", as.integer(doc_id))),  # Pastikan jadi 3 digit
    gamma = ifelse(is.na(gamma), 0, gamma)  # Gantilah NA dengan 0 agar tidak error
  )


p_bubble <- ggplot(topik_terdeteksi_11gibbs, aes(x = factor(topic), y = fct_rev(factor(doc_id)), size = gamma, color = gamma)) +
  geom_point(alpha = 0.8) +  
  scale_color_viridis_c(option = "D") +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0, size = 10),  # Posisi teks sumbu X
    axis.text.y = element_text(size = 10, margin = margin(t = 3, b = 3)),  # Jarak antar label Y
    axis.title.x = element_text(vjust = -2),  # Geser judul sumbu X agar sesuai di atas
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.title.x.top = element_text(size = 12),  # Atur label X di atas
    axis.text.x.top = element_text(size = 10),  # Atur teks sumbu X di atas
    axis.title.y = element_text(vjust = 2)  # Geser judul sumbu Y agar lebih jelas
  ) +
  scale_x_discrete(position = "top") +  # Pindahkan sumbu X ke atas
  scale_y_discrete() +  # Pastikan label Y dalam format diskrit
  facet_wrap(~ group, ncol = 2, nrow = 2, scales = "free_y") +  
  labs(
    title = "Bubble Plot: Sebaran Topik pada Tiap Dokumen",
    x = "Topic",
    y = "articles ID",
    size = "Gamma",
    color = "Gamma"
  )

print(p_bubble)
ggsave("heatmap_highres-topic11_mean-sd.jpg", p_bubble, width = 10, height = 14, dpi = 600)

View(doc_topics_filtered_11gibbs)
view(topik_terdeteksi_11gibbs)
write.csv2(doc_topics_filtered_11gibbs,"D:/011_git/002_text_mining/topic_modelling/topic-filtered-t11-0_06")
write.csv2(topik_terdeteksi_11gibbs,"D:/011_git/002_text_mining/topic_modelling/topic-filtered-t11-mean-sd")

####################################################################################
# Mengambil distribusi topik dari hasil LDA Gibbs
topic_distribution <- as.data.frame(lda_gibbs11@gamma)
View(topic_distribution)

# Ekstrak distribusi probabilitas artikel terhadap topik
doc_topics_11gibbs <- tidy(lda_gibbs11, matrix = "gamma")
View(doc_topics_11gibbs)


# Menambahkan kolom ID dokumen
topic_distribution$doc_id <- 1:nrow(topic_distribution)
View(topic_distribution)

##mengambil 3 topic per artikel##
top_3_topics_per_artikel <- doc_topics_11gibbs %>%
  group_by(document) %>%
  top_n(3, wt = gamma) %>%
  arrange(document, desc(gamma)) %>%
  ungroup()

View(top_3_topics_per_artikel)


# Hitung jumlah kemunculan tiap topik (berapa kali topik muncul dalam 3 besar tiap dokumen)
topic_counts <- top_3_topics_per_artikel %>%
  count(topic)

# Visualisasikan
ggplot(topic_counts, aes(x = factor(topic), y = n)) +
  geom_col(fill = "grey60") +
  labs(
    title = "Number of Articles per Topic (Top 3 topic per Article)",
    x = "Topic",
    y = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

hist(top_3_topics_per_artikel$gamma)

#change name of coloumn term into word on topic_model-11
top_3_topics_per_artikel <- top_3_topics_per_artikel %>%
  rename(doc_id = document)

typeof(top_3_topics_per_artikel$doc_id)
typeof(orangutannews16)

top_3_topics_per_artikel <- top_3_topics_per_artikel %>%
  mutate(doc_id = as.integer(doc_id))

# Menggabungkan hasil filtering topik ke raw data berdasarkan doc_id
top_3_orangutannews_11_topik <- orangutannews16 %>%
  left_join(top_3_topics_per_artikel, by = "doc_id")

write.csv2(top_3_orangutannews_11_topik,"D:/011_git/002_text_mining/topic_modelling/top_3_topics_per_artikel.csv")


library(lubridate)
# Konversi kolom date_publish menjadi format Date
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  mutate(date_publish01 = dmy(date_publish01)) # Format dari dd/mm/yyyy ke Date

View(top_3_orangutannews_11_topik)
View(orangutannews16)





########sebaran kata antar topik##########################3

library(tibble)

# **4. Buat matriks kata-topik untuk melihat tumpang tindih**
word_topic_matrix <- top_terms_gibbs11 %>%
  pivot_wider(names_from = topic, values_from = beta, values_fill = 0) %>%
  column_to_rownames("term")

# **5. Plot Heatmap untuk melihat sebaran kata per topik**
heatmap(as.matrix(word_topic_matrix), Colv = NA, scale = "column", margins = c(10, 10))

# **6. Cari kata yang muncul dalam lebih dari satu topik**
word_overlap <- top_terms %>%
  group_by(term) %>%
  summarise(n_topic = n_distinct(topic)) %>%
  filter(n_topic > 1)  # Pilih kata yang muncul di lebih dari satu topik

# **7. Tampilkan daftar kata yang tumpang tindih di beberapa topik**
print(word_overlap)
View(word_overlap)
write.csv2(word_overlap,"D:/011_git/002_text_mining/topic_modelling/word_overlap17topic.csv")


##########dendogram topic by the word########################

citation("ggplot2")
library(cluster)  # Untuk clustering
citation("cluster")
library(ggdendro) # Untuk visualisasi dendrogram dengan ggplot2
citation("ggdendro")


#3️⃣ Buat matriks kata-topik
word_topic_matrix <- top_terms_gibbs11 %>%
  pivot_wider(names_from = topic, values_from = beta, values_fill = 0) %>%
  column_to_rownames("term")

# 4️⃣ Hitung jarak antar topik
dist_matrix <- dist(t(word_topic_matrix), method = "euclidean")

# 5️⃣ Hierarchical clustering dan dendrogram
hc <- hclust(dist_matrix, method = "ward.D2")

# Visualisasi dendrogram dengan pengaturan font dan ketebalan garis
plot(hc, 
     main = "Dendrogram Topic LDA",  # Judul
     xlab = "Topic ID",                # Label sumbu X
     sub = "",                      # Subtitle
     cex = 1.2,                     # Ukuran teks keseluruhan
     cex.main = 1.5,                # Ukuran teks judul
     cex.lab = 1.3,                 # Ukuran label sumbu
     cex.axis = 1.1,                # Ukuran label sumbu X (topik)
     lwd = 2)                       # Ketebalan garis dendrogram

# Load library yang diperlukan
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("ggpattern", dependencies = TRUE)
library(ggpattern)
library(ggplot2)
library(magrittr)

colnames(top_3_orangutannews_11_topik)
# Menghapus kolom name_topic.x jika ada
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  select(-name_topic.x)
# Ganti nama kolom name_topic.y menjadi name_topic
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  rename(name_topic = name_topic.y)


# Tambahkan kolom kategori berdasarkan nomor topik
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  mutate(themes = case_when(
    topic %in% 1:6 ~ "human-orangutan interactions",
    topic %in% 7:11 ~ "mitigation and handling of negative interactions",
    TRUE ~ "other"
  ))

# Buat deskripsi manual tiap topik
topic_descriptions <- tibble(
  topic = 1:11,
  name_topic = c(
    "Orangutan on transportation access",
    "Orangutan sighting in community plantation",
    "Orangutan sighting in corporate plantation",
    "Orangutan captive",
    "Orangutan raiding crop",
    "Orangutan hunting",
    "Translocation orangutan",
    "Orangutan injured Condition",
    "Handover orangutan",
    "Wildlife captive regulation",
    "Population and Habitat Orangutan"
  ))

# Gabungkan deskripsi topik ke dalam data LDA
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  left_join(topic_descriptions, by = "topic")


# Filter data berdasarkan tahun 2011 hingga 2016
data_filtered <- top_3_orangutannews_11_topik %>%
  filter(year(date_publish01) >= 2011 & year(date_publish01) <= 2024)

# Hitung jumlah topik per tahun
topik_per_tahun <- data_filtered %>%
  count(year = year(date_publish01), name_topic.y)

# Buat salinan name_topic dengan pemisahan 2 kata pertama lalu ganti jadi 2 baris
topik_per_tahun <- topik_per_tahun %>%
  mutate(name_topic_wrapped = str_replace(name_topic.y, 
                                          "^((\\S+\\s+){2})", "\\1\n"))


topik_per_tahun$name_topic.y <- as.factor(topik_per_tahun$name_topic.y)

# Tambahkan kolom themes berdasarkan nomor topik
top_3_orangutannews_11_topik <- top_3_orangutannews_11_topik %>%
  mutate(themes = case_when(
    topic %in% 1:6 ~ "Human-orangutan interactions",
    topic %in% 7:11 ~ "Mitigation and handling of negative interactions",
    TRUE ~ "other"
  ))



library(ggplot2)
library(dplyr)
library(stringr)

# Tambah kolom themes ke topik_per_tahun
topik_per_tahun01 <- data_filtered %>%
  count(year = year(date_publish01), topic) %>%
  left_join(topic_descriptions, by = "topic") %>%
  left_join(
    top_3_orangutannews_11_topik %>% select(topic, themes) %>% distinct(),
    by = "topic"
  ) %>%
  mutate(
    name_topic_wrapped = str_replace(name_topic, "^((\\S+\\s+){2})", "\\1\n")
  )
View(topik_per_tahun01)

# Buat lookup unik dari topic ke name_topic_wrapped
levels_lookup <- topik_per_tahun01 %>%
  select(topic, name_topic_wrapped) %>%
  distinct() %>%
  arrange(topic) %>%
  pull(name_topic_wrapped)

# Terapkan level unik tersebut ke faktor
topik_per_tahun02 <- topik_per_tahun01 %>%
  mutate(name_topic_wrapped = factor(name_topic_wrapped, levels = levels_lookup))
view(topik_per_tahun02)

# Atur urutan topik berdasarkan themes (untuk visual group)
topik_per_tahun03 <- topik_per_tahun02 %>%
  arrange(factor(themes, levels = c(
    "Human-orangutan interactions",
    "Mitigation and handling of negative interactions",
    "other"
  )), topic) %>%
  mutate(name_topic_wrapped = factor(name_topic_wrapped, levels = rev(unique(name_topic_wrapped))),
         themes = factor(themes))
View(topik_per_tahun03)

# Ambil posisi garis pemisah antar themes
breaks_pos <- topik_per_tahun03 %>%
  distinct(name_topic_wrapped, themes) %>%
  group_by(themes) %>%
  summarise(n = n()) %>%
  mutate(pos = cumsum(n) + 0.5) %>%
  filter(row_number() < n())
View(breaks_pos)

# Buat posisi y untuk label themes (kanan)
theme_label_pos <- topik_per_tahun03 %>%
  distinct(name_topic_wrapped, themes) %>%
  group_by(themes) %>%
  summarise(pos = mean(as.numeric(name_topic_wrapped))) %>%
  mutate(
    label = str_wrap(as.character(themes), width = 20)
  )

View(theme_label_pos)

# Plot utama
ggplot(topik_per_tahun03, aes(x = factor(year), y = name_topic_wrapped, fill = n)) +
  geom_tile(color = "white") +
  
  # Garis horizontal pemisah tema
  geom_hline(data = breaks_pos, aes(yintercept = 5.5), color = "black", linewidth = 0.5) +
  
  # Formating: Tambah label kategori di kanan (themes)
  geom_text(data = theme_label_pos, 
            aes(x = max(as.numeric(as.factor(topik_per_tahun03$year))) + 0.6, 
                y = pos, label = label), 
            inherit.aes = FALSE,
            hjust = 0, fontface = "bold", color = "black", size = 4.5) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    values = scales::rescale(c(0, 2, 4, 6, 8, 10)),
    na.value = "grey90",
    limits = range(topik_per_tahun03$n, na.rm = TRUE)
  ) +
  labs(
    x = "Year",
    y = "Topic",
    fill = "Quantity",
    title = "Heatmap Latent Topic Distribution"
  ) +
  coord_cartesian(clip = "off") +  # supaya label di luar plot terlihat
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(10, 100, 10, 10)  # beri ruang di kanan
  )
























