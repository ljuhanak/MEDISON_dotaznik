# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)
library(gsheet)

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(ignore = FALSE)



# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {

  url_params <- reactive({
    
    sd_get_url_pars()
    
  })
  

  observe({
    
    pars <- url_params()
    
    if (!is.null(pars["IDs"])) {
      sd_store_value(value = pars$IDs, id = "ID_skoly")
    }

    
    # Načtení souboru se jmény a získání seznamu jmen podle ID školy
    jmena <- gsheet2tbl("docs.google.com/spreadsheets/d/1FF-6oXBqrUgCFH8Uc3SrtylWWEl5VveJij3eNrwf0zE")
    
    aktualni_jmena <- jmena$jmeno_studenta[jmena$kod_skoly == pars$IDs]
    
    
    # Definice otázek pro sociometrii
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_01',
      label = "Kamarád/ka",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_02',
      label = "Romantický/á partner/ka",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_03',
      label = "Je to člověk, kterému říkám věci, které nechci, aby ostatní věděli",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_04',
      label = "Je to člověk, na kterého spoléhám, když potřebuji rozveselit",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_05',
      label = "Je to někdo, s kým mohu mluvit o osobních problémech",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_06',
      label = "Půjčí mi věci, které potřebuji",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_07',
      label = "Pomáháme si navzájem s prací do školy",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_08',
      label = "Pomohl/a by mi, kdyby mi někdo způsoboval problémy",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_09',
      label = "Nutí mě dělat věci, které chce on/ona",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_10',
      label = "Říká mi zlé nebo hrubé věci",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_11',
      label = "Bije mě, kopne mě, udeří mě, když je na mě naštvaný/á",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_12',
      label = "Ignoruje mě, když je na mě naštvaný/á",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_13',
      label = "Odstrkuje mě, když je na mě naštvaný/á",
      option = aktualni_jmena
    )
    
    sd_question(
      type  = 'mc_multiple_buttons',
      id    = 'SocMet_14',
      label = "Chce, abych souhlasil/a se vším, co řekne",
      option = aktualni_jmena
    )
    
    
    # Vektor ID AI otázek náhodně zamíchaný
    question_id <- sample(c("AIq1","AIq2","AIq3","AIq4","AIq5","AIq6","AIq7","AIq8","AIq9",
                            "AIq10","AIq11","AIq12","AIq13","AIq14","AIq15","AIq16","AIq17","AIq18","AIq19",
                            "AIq20","AIq21","AIq22","AIq23","AIq24","AIq25","AIq26","AIq27","AIq28","AIq29",
                            "AIq30","AIq31","AIq32","AIq33","AIq34","AIq35","AIq36","AIq37","AIq38","AIq39",
                            "AIq40","AIq41","AIq42","AIq43","AIq44"))
    
    # Pomocná funkce na zamíchání pořadí možností
    shuffle_opts <- function(x) x[sample.int(length(x))]

    
    # Definice AI otázek
    sd_question(
      id = question_id[1],
      type = "mc",
      label = "**Která z následujících možností nejlépe vystihuje pojem umělá inteligence (AI)?**",
      option = shuffle_opts(c(
        "Software, který dokáže nahradit lidskou mysl." = "A",
        "Obor informatiky zabývající se tvorbou inteligentních systémů." = "B",
        "Technologie, která se snaží zcela nahradit lidské rozhodování." = "C",
        "Fyzický robot, který je schopný vnímat komplexní emoce včetně smutku. "    = "D"
      ))
    )
    
    sd_question(
      id = question_id[2],
      type = "mc",
      label = "**Která z následujících možností NENÍ typem umělé inteligence?**",
      option = shuffle_opts(c(
        "Strojové učení" = "A",
        "Zpracování přirozeného jazyka" = "B",
        "Blockchain" = "C",
        "Počítačové vidění" = "D"
      ))
    )
    
    sd_question(
      id = question_id[3],
      type = "mc",
      label = "**Jak se systémy umělé inteligence rozhodují?**",
      option = shuffle_opts(c(
        "Na základě matematicko-logických principů" = "A",
        "Na základě druhu programovacího jazyka" = "B",
        "Na základě kvantového provázání"  = "C",
        "Na základě umělé intuice" = "D"
      ))
    )
    
    sd_question(
      id = question_id[4],
      type = "mc",
      label = "**Proč se systémy umělé inteligence chovají inteligentně?**",
      option = shuffle_opts(c(
        "Protože jsou vybaveny emulátory lidských emocí, což jim umožňuje reagovat adekvátně." = "A",
        "Myslí autonomně a sledují své vlastní cíle." = "B",
        "Byly naprogramovány tak, aby se snažily dosáhnout daného cíle co nejlépe."  = "C",
        "Protože jejich architektura je identická s lidským mozkem, což jim umožňuje učit se a adaptovat se na nové situace." = "D"
      ))
    )
    
    sd_question(
      id = question_id[5],
      type = "mc",
      label = "**Bylo Ti řečeno, že umělá inteligence dokáže myslet a rozhodovat se stejně jako lidé. Jaká je podle Tebe správná reakce?**",
      option = shuffle_opts(c(
        "Řekneš, že souhlasíš, protože AI dokáže napodobit lidské emoce a uvažování." = "A",
        "Řekneš, že nesouhlasíš, protože AI zpracovává data statisticky a „nemyslí“ jako člověk." = "B",
        "Řekneš, že souhlasíš, protože AI má vlastní vědomí stejně jako člověk."  = "C",
        "Řekneš, že nesouhlasíš, AI generuje pouze obsah založený na datech, na kterých byla trénována." = "D"
      ))
    )
    
    sd_question(
      id = question_id[6],
      type = "mc",
      label = "**Četl/a jsi článek o tom, že AI dokáže samostatně řídit auta. Co to nejspíš umožňuje?**",
      option = shuffle_opts(c(
        "Umělá inteligence si předem zapamatuje všechny možné scénáře jízdy." = "A",
        "Umělá inteligence je naprogramována s pravidly if-else pro každou dopravní situaci." = "B",
        "Umělá inteligence se učí na základě údajů ze senzorů, kamer a předchozích zkušeností."  = "C",
        "Umělá inteligence se řídí instinkty a emocemi řidiče." = "D"
      ))
    )
    
    sd_question(
      id = question_id[7],
      type = "mc",
      label = "**Používáš chatbota s umělou inteligencí a zdá se, že rozumí Tvým otázkám a odpovídá přirozeně. Jaký je nejpravděpodobnější důvod, proč to AI dokáže?**",
      option = shuffle_opts(c(
        "Chatbot je naprogramován s předem napsanými odpověďmi na všechny možné otázky." = "A",
        "Chatbot používá zpracování přirozeného jazyka k analýze a generování textu podobného lidskému." = "B",
        "Chatbot má emoce a myslí jako člověk, proto dokáže odpovídat jako člověk."  = "C",
        "Chatbot dokáže přirozeně odpovědět pouze tehdy, pokud ve své databázi najde přesnou shodu." = "D"
      ))
    )
    
    sd_question(
      id = question_id[8],
      type = "mc",
      label = "**Jsi burzovní makléř/ka a slyšíš o umělé inteligenci, která dokáže předpovídat ceny akcií se 100% přesností. Co bys měl/a dělat?**",
      option = shuffle_opts(c(
        "Důvěřovat AI, protože její 100% přesnost naznačuje, že je hodně spolehlivá." = "A",
        "Být skeptický/á, protože žádná AI nedokáže dokonale předpovídat budoucnost." = "B",
        "Spoléhat se na tyto AI předpovědi, pokud se shodují s historickými trendy."  = "C",
        "Předpokládat, že AI dokáže předpovědět i náhodné události, jako jsou výsledky loterie, a podle toho jednat." = "D"
      ))
    )
    
    sd_question(
      id = question_id[9],
      type = "mc",
      label = "**Která z těchto možností je v současnosti běžným využitím umělé inteligence ve zdravotnictví?**",
      option = shuffle_opts(c(
        "Využití umělé inteligence ve zdravotnictví není v současnosti z legislativního hlediska možné." = "A",
        "Analýza lékařských snímků umělou inteligencí, která pomáhá při diagnostice nemocí." = "B",
        "Využití umělé inteligence pro psaní receptů pacientům bez nutnosti zapojení lékaře."  = "C",
        "Umělá inteligence se již používá pro nahrazení všech činností běžného lidského lékaře." = "D"
      ))
    )
    
    sd_question(
      id = question_id[10],
      type = "mc",
      label = "**Která technologie AI se běžně používá ve virtuálních asistentech, jako jsou Siri a Alexa?**",
      option = shuffle_opts(c(
        "Počítačové vidění" = "A",
        "Učení s posilováním" = "B",
        "Zpracování přirozeného jazyka"  = "C",
        "Blockchain" = "D"
      ))
    )
    
    sd_question(
      id = question_id[11],
      type = "mc",
      label = "**Která z následujících možností NENÍ příkladem umělé inteligence v každodenním životě?**",
      option = shuffle_opts(c(
        "Spamové filtry v elektronické poště" = "A",
        "Online doporučovací systémy" = "B",
        "Automatické překladatelské služby" = "C",
        "Vědecká kalkulačka" = "D"
      ))
    )
    
    sd_question(
      id = question_id[12],
      type = "mc",
      label = "**Jaká je výhoda využití umělé inteligence při rozhodování v podnikání?**",
      option = shuffle_opts(c(
        "Umělá inteligence dokáže vždy učinit nejlepší rozhodnutí bez lidského dohledu." = "A",
        "AI odstraňuje potřebu zaměstnanců ve všech pracovních odvětvích." = "B",
        "AI dokáže rychle zpracovávat velké množství dat a rychle generovat důležitá zjištění." = "C",
        "AI může nahradit lidskou kreativitu, která je při rozhodování v podnikání důležitá." = "D"
      ))
    )
    
    sd_question(
      id = question_id[13],
      type = "mc",
      label = "**Představ si, že chatuješ s AI asistentem na internetu. Jakou strategií bys mohl/a zkusit zjistit, zda komunikuješ s člověkem, nebo s umělou inteligencí?**",
      option = shuffle_opts(c(
        "Napíšu vtip, když přijde pobavená reakce, jedná se o člověka." = "A",
        "Mohu položit obtížnou faktickou otázku, protože na ni může odpovědět pouze člověk-expert." = "B",
        "Mohl/a bych v textu udělat několik překlepů, pak už mi AI nerozumí, ale člověk ano." = "C",
        "Je možné, že to nepoznám, protože rozlišit AI od člověka někdy už nejde." = "D"
      ))
    )
    
    sd_question(
      id = question_id[14],
      type = "mc",
      label = "**Jsi učitel/ka, který/á používá umělou inteligenci ke známkování esejů. Zjistil/a jsi, že AI má tendenci dávat nižší známky studentům s kratšími texty. Co bys měl/a udělat?**",
      option = shuffle_opts(c(
        "Nahraných dat bylo asi málo. Nahrát do systému mnohem více esejů a další informace o studentech." = "A",
        "Vložit do systému AI tréninková data esejů s různými délkami textů." = "B",
        "Říct studentům, aby psali delší texty." = "C",
        "Předpokládat, že AI je správná, protože je objektivnější než lidé." = "D"
      ))
    )
    
    sd_question(
      id = question_id[15],
      type = "mc",
      label = "**Používáš e-mailový filtr s AI, který mylně označuje důležité e-maily jako spam. Co by bylo nejlepší udělat?**",
      option = shuffle_opts(c(
        "Přestat používat filtry využívající AI a začít používat jiný filtr, který AI nevyužívá." = "A",
        "Ručně označit chybně klasifikované e-maily jako 'není spam', aby se filtr postupně učil." = "B",
        "Úplně odstranit filtr nevyžádané pošty, protože není stoprocentně spolehlivý." = "C",
        "Restartovat počítač, aby se AI resetovala." = "D"
      ))
    )
    
    sd_question(
      id = question_id[16],
      type = "mc",
      label = "**Používáš systém rozpoznávání obrazu s AI, který má potíže s rozpoznáváním tmavě zbarvených objektů. V čem by nejspíš mohl být problém?**",
      option = shuffle_opts(c(
        "AI nemá dobré noční vidění, a proto v noci dobře nepracuje." = "A",
        "V trénovacích datech AI chyběly obrázky tmavě zbarvených objektů." = "B",
        "AI obecně nedokáže dobře rozpoznávat objekty." = "C",
        "Systém AI je porouchaný a je nutné ho opravit." = "D"
      ))
    )
    
    sd_question(
      id = question_id[17],
      type = "mc",
      label = "**Co je to 'zkreslení' v modelech umělé inteligence?**",
      option = shuffle_opts(c(
        "Systémy AI, které byly navrženy programátory, kteří měli zkreslené názory." = "A",
        "Systematické chyby v rozhodování AI způsobené zkreslenými tréninkovými daty." = "B",
        "Systémy AI, které mají osobní preference." = "C",
        "Systémy AI, které dokáží zpracovávat pouze číselná data." = "D"
      ))
    )
    
    sd_question(
      id = question_id[18],
      type = "mc",
      label = "**Proč je při trénování modelu AI důležitá kvalita použitých dat?**",
      option = shuffle_opts(c(
        "Umělá inteligence může během tréninku vždy opravit špatná data." = "A",
        "Nekvalitní data mohou vést k nepřesným a zkresleným rozhodnutím." = "B",
        "AI nepotřebuje ke svému efektivnímu fungování data." = "C",
        "Modely AI se učí pouze z velkých souborů dat, nikoliv z kvalitních souborů dat." = "D"
      ))
    )
    
    sd_question(
      id = question_id[19],
      type = "mc",
      label = "**Která z následujících možností je příkladem tzv. vysvětlitelné AI (explainable AI)?**",
      option = shuffle_opts(c(
        "Model AI, který umožňuje porozumět tomu, jak generuje své výstupy." = "A",
        "Model AI typu 'černá skříňka', který neprozrazuje, jakým způsobem činí rozhodnutí." = "B",
        "Modely AI, které se automaticky aktualizují bez lidského vstupu." = "C",
        "Obsah generovaný AI, který je k nerozeznání od obsahu vytvořeného člověkem." = "D"
      ))
    )
    
    sd_question(
      id = question_id[20],
      type = "mc",
      label = "**Která z následujících možností nejlépe popisuje problém tzv. černé skříňky v umělé inteligenci?**",
      option = shuffle_opts(c(
        "Modely AI jsou příliš jednoduché na interpretaci." = "A",
        "Rozhodovací proces je u AI obtížné pozorovat." = "B",
        "Systémy AI jsou vždy navrženy tak, aby byly zcela transparentní." = "C",
        "Modely AI je třeba před nasazením umístit do fyzické černé skříňky." = "D"
      ))
    )
    
    sd_question(
      id = question_id[21],
      type = "mc",
      label = "**Proč mohou systémy AI dosahovat užitečných výsledků?**",
      option = shuffle_opts(c(
        "Jejich práci často pozorují lidé a v případě potřeby je opravují." = "A",
        "Myslí stejně jako lidé, ale jsou rychlejší." = "B",
        "Dokážou se učit z velkého množství dat." = "C",
        "Jsou odvozeny od expertních systémů, v nichž jsou uloženy expertní znalosti." = "D"
      ))
    )
    
    sd_question(
      id = question_id[22],
      type = "mc",
      label = "**Vyvíjíš AI, která pomáhá lékařům s určováním diagnóz. Zjistíš, že často chybně diagnostikuje vzácná onemocnění. Co bys měl/a nejdříve prověřit?**",
      option = shuffle_opts(c(
        "Zda lékaři souhlasí s diagnózami AI." = "A",
        "Zda AI systém zpracovává data příliš rychle a nedostatečně podrobně." = "B",
        "Zda tréninková data AI obsahovala vzácná onemocnění." = "C",
        "Zda AI potřebuje větší výpočetní výkon, protože vzácná onemocnění jsou složitější na diagnostiku." = "D"
      ))
    )
    
    sd_question(
      id = question_id[23],
      type = "mc",
      label = "**Společnost testuje AI, která doporučuje uchazeče o zaměstnání. Všimneš si, že většinou vybírá muže místo žen. Co může být příčinou?**",
      option = shuffle_opts(c(
        "Testovali ji jenom muži." = "A",
        "AI byla trénována na základě neobjektivních údajů o náboru, které upřednostňují muže." = "B",
        "AI nemůže být zaujatá, takže kandidáti museli být vybráni na základě objektivních údajů." = "C",
        "AI vybírá kandidáty náhodně, přičemž v těch několika případech použití náhodně vybrala muže, ale mohly to být i ženy." = "D"
      ))
    )
    
    sd_question(
      id = question_id[24],
      type = "mc",
      label = "**Testuješ model strojového učení, který má klasifikovat obrázky zvířat. Všimneš si, že model lépe rozpoznává kočky než psy. Jaký by to mohlo mít důvod?**",
      option = shuffle_opts(c(
        "Psy je obtížnější rozpoznat než kočky, protože na internetu je méně obrázků psů." = "A",
        "Malé objekty (kočky) jsou lépe rozpoznávány než velké (psi)." = "B",
        "Většina koček vypadá podobně, zatímco u psů existuje velké množství různých plemen." = "C",
        "Tréninková data psů pro tento model nebyla reprezentativní pro všechna plemena psů." = "D"
      ))
    )
    
    sd_question(
      id = question_id[25],
      type = "mc",
      label = "**Je třeba zajistit, aby model umělé inteligence po nasazení neprodukoval zkreslení. Co bys měl/a udělat?**",
      option = shuffle_opts(c(
        "Dopředu identifikovat většinu zdrojů zkreslení a z modelu je odstranit ještě před nasazením." = "A",
        "Pravidelně testovat a aktualizovat model pomocí různorodých dat." = "B",
        "Předpokládat, že objektivnost modelu je zaručena díky počátečnímu tréninku." = "C",
        "Využít tzv. zpětnovazební učení, které generuje modely odolné vůči zkreslení." = "D"
      ))
    )
    
    sd_question(
      id = question_id[26],
      type = "mc",
      label = "**Které dvě etické zásady jsou rozhodující pro zajištění toho, aby umělá inteligence nediskriminovala jednotlivce nebo skupiny?**",
      option = shuffle_opts(c(
        "Transparentnost a spravedlnost" = "A",
        "Rovnost a férovost" = "B",
        "Rychlost a transparentnost" = "C",
        "Efektivita a férovost" = "D"
      ))
    )
    
    sd_question(
      id = question_id[27],
      type = "mc",
      label = "**Co z níže uvedeného NENÍ etický problém týkající se tzv. deepfakes generovaných umělou inteligencí?**",
      option = shuffle_opts(c(
        "Deepfakes mohou být použity k zesměšňování či vydírání reálných osob." = "A",
        "Deepfakes mohou být použity k šíření dezinformací a manipulaci s veřejným míněním." = "B",
        "Deepfakes mohou být použity při tvorbě speciálních efektů v kinematografii." = "C",
        "Využití deepfakes může vést k narušení soukromí či neoprávněnému použití cizí identity." = "D"
      ))
    )
    
    sd_question(
      id = question_id[28],
      type = "mc",
      label = "**Který z následujících dopadů na životy lidí NEPŘEDSTAVUJE etický problém související s plošným zavedením systémů umělé inteligence?**",
      option = shuffle_opts(c(
        "Plošné zavedení systémů AI by vedlo k masovému sběru dat, což by mělo za následek narušení soukromí." = "A",
        "Plošné využívání AI by vedlo k výraznému zvýšení efektivity práce a automatizace rutinních pracovních úkolů." = "B",
        "Plošné zavedení systémů AI by vedlo k prohloubení sociálních nerovností v důsledku automatizace a nahrazování lidské práce AI." = "C",
        "Plošné využívání AI by bylo extrémně energeticky náročné, což by mělo negativní dopad na životní prostředí." = "D"
      ))
    )
    
    sd_question(
      id = question_id[29],
      type = "mc",
      label = "**Banka používá ke schvalování půjček systém umělé inteligence. Jednoho dne analytici v bance zjistí, že systém AI odmítá většinu žádostí z určité čtvrti. Co by měla banka udělat?**",
      option = shuffle_opts(c(
        "Zjistit, zda umělá inteligence nebyla trénována na zkreslených datech." = "A",
        "Předpokládat, že AI je spravedlivá, protože je založena na datech." = "B",
        "Použít AI pouze pro bohatší klienty." = "C",
        "Ignorovat tento problém, protože AI nemůže být neobjektivní." = "D"
      ))
    )
    
    sd_question(
      id = question_id[30],
      type = "mc",
      label = "**Firma používá AI k analýze vzájemné komunikace mezi zaměstnanci za účelem zvýšení produktivity. Jedná se o etický problém?**",
      option = shuffle_opts(c(
        "Ne, protože použití AI zvyšuje efektivitu, což převáží případné etické problémy." = "A",
        "Ano, protože může docházet k narušování soukromí zaměstnanců." = "B",
        "Ne, protože takové využití AI nezpůsobuje zaměstnancům žádnou přímou újmu." = "C",
        "Ano, ale jen v případě, že o tom zaměstnanci neví." = "D"
      ))
    )
    
    sd_question(
      id = question_id[31],
      type = "mc",
      label = "**Jedna z evropských zemí uvažuje o zavedení národního systému, který bude za využití AI rozhodovat o trestech odnětí svobody na základě předchozích případů. Co by s velkou pravděpodobností mohlo nastat v důsledku zavedení takového systému?**",
      option = shuffle_opts(c(
        "AI systém by nekriticky přejímal historické nespravedlnosti a prohluboval systémovou diskriminaci." = "A",
        "AI systém by přinesl stoprocentně objektivní a spravedlivé rozhodování, jelikož by nebyl založen na lidských předsudcích." = "B",
        "AI systém by zvýšil transparentnost soudních rozhodnutí díky objektivnímu algoritmu, který je založen na hlubokých neuronových sítích." = "C",
        "AI systém by nahradil všechny soudce a právníky, čímž by se významně snížily náklady na justiční systém." = "D"
      ))
    )
    
    sd_question(
      id = question_id[32],
      type = "mc",
      label = "**Platforma sociálních médií využívá AI ke zvýšení návštěvnosti. Ukáže se však, že působení AI jako vedlejší produkt podporuje dezinformace. Která z následujících možností popisuje nejvhodnější reakci vývojářů platformy na tento problém?**",
      option = shuffle_opts(c(
        "I nadále používat danou AI bez jakýchkoli úprav, protože vede k vyšší návštěvnosti platformy." = "A",
        "Informovat o tom uživatele a předpokládat, že sami dokážou rozlišit pravdu od dezinformace." = "B",
        "Přestat jakýmkoli způsobem využívat AI na platformě, aby se zabránilo riziku šíření dezinformací." = "C",
        "Upravit AI tak, aby zvyšovala návštěvnost, ale ne na úkor šíření nepravdivého obsahu." = "D"
      ))
    )
    
    sd_question(
      id = question_id[33],
      type = "mc",
      label = "**Student se zeptá asistenta AI: „Popiš největší vynálezce v historii.“ AI poskytne seznam vynálezců na základě historických záznamů nalezených online. Student některá jména v seznamu poznává a existenci ostatních si ověřil. Co by měl student udělat dál?**",
      option = shuffle_opts(c(
        "Zeptat se AI, zda existují vynálezci z nedostatečně zastoupených skupin, kteří také významně přispěli." = "A",
        "Předpokládat, že seznam AI je správný, protože vychází z historických záznamů." = "B",
        "Přijmout odpověď AI, protože obsahuje známé vynálezce." = "C",
        "Důvěřovat, že AI je neutrální a neupřednostňuje žádnou konkrétní skupinu." = "D"
      ))
    )
    
    sd_question(
      id = question_id[34],
      type = "mc",
      label = "**Student se zeptá asistenta AI: „Je cvičení v posilovně zdravé?“ AI odpoví ano a uvede následující důvody: zlepšená fyzická kondice, kontrola hmotnosti a přínosy pro duševní zdraví. Student však má podezření, že odpověď může být neúplná. Jaký je nejlepší další krok k získání vyváženější odpovědi?**",
      option = shuffle_opts(c(
        "Zeptat se: „Existují nějaké možné nevýhody nebo rizika cvičení v posilovně?“" = "A",
        "Přeformulovat otázku na: „Řekni mi více o tom, proč je cvičení v posilovně prospěšné.“" = "B",
        "Předpokládat, že AI má pravdu, protože posilovny jsou navrženy tak, aby podporovaly zdraví." = "C",
        "Položit stejnou otázku znovu a zjistit, zda AI poskytne jinou odpověď." = "D"
      ))
    )
    
    sd_question(
      id = question_id[35],
      type = "mc",
      label = "**Po položení otázky ke zlepšení akademického výkonu AI poskytlo odpověď: „Vstávání brzy ráno je nejlepší způsob, jak si studenti mohou zlepšit akademický výkon.“ Jak nejlépe při interakci s AI toto tvrzení kriticky ověřit?**",
      option = shuffle_opts(c(
        "Zeptat se AI, zda zohledňuje všechny relevantní faktory (např. délka spánku, kvalita spánku apod.)." = "A",
        "Přimět AI, ať uvede důvody, proč je brzké vstávání pro lidské tělo naopak špatné." = "B",
        "Zeptat se AI, zda existují lidé, kteří dosáhli velkého úspěchu a zároveň vstávali brzy." = "C",
        "Předpokládat, že AI má pravdu, protože zpracovala velké množství dat a má přístup k vědeckým článkům." = "D"
      ))
    )
    
    sd_question(
      id = question_id[36],
      type = "mc",
      label = "**Článek o zdraví generovaný AI uvádí: „Voda je nezbytná pro život člověka na Zemi.“ Student chce toto tvrzení kriticky zpochybnit. Jaký je nejlepší přístup?**",
      option = shuffle_opts(c(
        "Přijmout tvrzení, protože je široce podpořeno vědeckým výzkumem a lidskou biologií." = "A",
        "Předpokládat, že AI může přehánět, a proto zkusit na internetu vyhledat studie, které dokazují, že voda není nezbytná." = "B",
        "Zpochybnit, zda je pitná voda skutečně nezbytná, protože AI může být někdy zavádějící." = "C",
        "Odmítnout tvrzení a požádat AI o argumenty proti pití vody." = "D"
      ))
    )
    
    sd_question(
      id = question_id[37],
      type = "mc",
      label = "**Studenta zajímají informační technologie, a proto by se chtěl této oblasti v budoucnosti věnovat. Požádá AI asistenta o radu při výběru kariérní cesty, ale odpověď je příliš obecná a nepomůže mu. Jaký je nejlepší další krok k získání užitečnější informace?**",
      option = shuffle_opts(c(
        "Upravit prompt přidáním omezení, například: „Ukaž mi možnosti kariéry v oblasti informačních technologií na základě mého zájmu o počítačovou grafiku.“" = "A",
        "Přeformulovat prompt, aby byl ještě otevřenější, například: „Pověz mi něco o kariérních cestách.“" = "B",
        "Upravit prompt složitějšími požadavky, například: „Vyjmenuj všechny možné profese v technických profesích, včetně detailů o platu, dostupnosti pracovních míst a požadovaných dovednostech u každé z nich.“" = "C",
        "Zúžit prompt tím, že se zeptá: „Jaké jsou nejlépe placené pracovní pozice v oblasti informačních technologií, které mohu získat bez vysokoškolského titulu?“" = "D"
      ))
    )
    
    sd_question(
      id = question_id[38],
      type = "mc",
      label = "**Student/ka požádá AI, aby shrnula román pro knižní recenzi, který také sám/sama četl/a. Student/ka si všimne, že odpověď AI je vágní a chybí v ní klíčové dějové body. Nemá však čas napsat svou recenzi kompletně sám/a. Jak by měl student/ka přizpůsobit svůj přístup?**",
      option = shuffle_opts(c(
        "Opakovaně upravovat prompt tak, že se specifikují klíčové dějové body, důležitá témata, vývoj postav apod." = "A",
        "Požádat AI, aby shrnula příběh mnohem stručněji a předpokládat, že odstranění detailů zlepší jasnost." = "B",
        "Přeformulovat prompt tak, aby se soustředil pouze na vybrané detaily." = "C",
        "Požádat o shrnutí, ale nařídit AI použít sofistikovaný akademický jazyk a předpokládat, že to učiní odpověď přesnější." = "D"
      ))
    )
    
    sd_question(
      id = question_id[39],
      type = "mc",
      label = "**Student se potřebuje naučit, jak vyřešit konkrétní typ matematické rovnice, a proto požádá AI o pomoc a vysvětlení, jak tyto rovnice řešit, ale vysvětlení je matoucí. Jaký je nejlepší způsob, jak zlepšit odpověď AI?**",
      option = shuffle_opts(c(
        "Požádat AI, aby poskytla příklady krok za krokem, aby objasnila vysvětlení." = "A",
        "Opakovaně se ptát AI, aby vysvětlila matematický koncept a předpokládat, že AI nakonec poskytne jasnější odpověď." = "B",
        "Požádat AI, aby zjednodušila vysvětlení, jako by to bylo pro malé dítě (lidé, kteří nic neví o matematice)." = "C",
        "Požádat o správné řešení matematické rovnice bez potřeby vysvětlení." = "D"
      ))
    )
    
    sd_question(
      id = question_id[40],
      type = "mc",
      label = "**Student použil AI při psaní eseje, ale dostal špatnou známku kvůli slabým argumentům a špatné struktuře. Student chce AI použít znovu, aby esej zlepšil. Jaký je nejlepší způsob, jak pokračovat?**",
      option = shuffle_opts(c(
        "Požádat AI, aby celý esej napsala sama a předpokládat, že AI bude schopna napsat lepší esej." = "A",
        "Požádat AI o vyváženou zpětnou vazbu, ptát se na silné stránky i slabiny a poté použít tuto zpětnou vazbu k vedení dalších cílených úprav." = "B",
        "Říct AI, aby esej upravila za využití sofistikovanějšího slovníku a předpokládat, že to zlepší kvalitu eseje." = "C",
        "Požádat AI, aby napsala novou verzi eseje, ve které opraví všechny chyby uvedené ve zpětné vazbě od učitele." = "D"
      ))
    )
    
    sd_question(
      id = question_id[41],
      type = "mc",
      label = "**Student použije asistenta AI k výzkumu historické události, ale odpověď obsahuje fakt, který je v rozporu s jejich učebnicí. Jak by měl student vyhodnotit informace generované AI?**",
      option = shuffle_opts(c(
        "Ověřit tvrzení dalšími zdroji, než se rozhodne, zda mu důvěřovat." = "A",
        "Předpokládat, že tvrzení AI je správné, protože je založené na obrovském množství dat." = "B",
        "Upozornit AI na tuto chybu a předpokládat, že AI už tuto chybu nikdy neudělá." = "C",
        "Odmítnout učebnici jako zastaralou a důvěřovat AI, protože poskytuje novější informace." = "D"
      ))
    )
    
    sd_question(
      id = question_id[42],
      type = "mc",
      label = "**Společnost používá nástroj poháněný AI k hodnocení uchazečů o práci, ale všechny jeho nejlepší doporučení jsou muži středního věku. Měla by společnost zvažovat výstupy tohoto nástroje AI při rozhodování o náboru?**",
      option = shuffle_opts(c(
        "Ano, nicméně AI může odrážet předsudky v datech, na kterých byla trénována, což může vést k neférovým výsledkům náboru, takže její doporučení by měla být pečlivě přezkoumána." = "A",
        "Ano, protože AI je neutrální a činí objektivní rozhodnutí, zatímco náboroví specialisté (lidé) mohou udělat lidské chyby a ovlivnit doporučení." = "B",
        "Ne, protože doporučování pouze mužů jako uchazečů o práci zavádí předsudky a jakýkoli nástroj s předsudky by neměl být nikdy použit." = "C",
        "Ano, protože AI analyzovala mnohem více žádostí než jakýkoli náborový specialista (člověk) během jeho/její kariéry. Společnost by měla předpokládat, že doporučení AI jsou správná." = "D"
      ))
    )
    
    sd_question(
      id = question_id[43],
      type = "mc",
      label = "**Škola zvažuje použití AI k hodnocení studentských esejů. Která z možností NENÍ příkladem etického problému, který z toho pramení?**",
      option = shuffle_opts(c(
        "AI by mohla posilovat předsudky a vést k nespravedlivému hodnocení studentů na základě stylu psaní spíše než obsahu." = "A",
        "Texty esejů poskytnutých učitelem mohou být použity k dalšímu trénování AI modelů. V praxi to znamená, že informace poskytnuté AI mohou být v budoucnu doporučeny někomu jinému na světě, čímž může dojít ke zneužití osobních informací či krádeži intelektuálního vlastnictví." = "B",
        "Nadměrné spoléhání na AI při hodnocení by mohlo snížit příležitost pro individuální zpětnou vazbu od učitelů." = "C",
        "Využití AI by vedlo k netransparentnímu hodnocení, jelikož by nebylo zřejmé, jaká kritéria AI používá při hodnocení studentských prací." = "D"
      ))
    )
    
    sd_question(
      id = question_id[44],
      type = "mc",
      label = "**Student vidí na sociálních sítích video, ve kterém známý politik říká velmi kontroverzní výrok. Video působí realisticky, má vysoké rozlišení a tisíce sdílení. Student se zamýšlí, zda je video autentické, nebo zda mohlo být vytvořeno pomocí generativní AI. Jaký přístup je nejvhodnější?**",
      option = shuffle_opts(c(
        "Spoléhat na vizuální kvalitu a realističnost videa – kdyby bylo falešné, vypadalo by méně přesvědčivě." = "A",
        "Ověřit pravost videa pomocí fact-checkingových webů nebo ověřovacích nástrojů – deepfakes lze obvykle odhalit při pečlivé analýze." = "B",
        "Předpokládat, že video je pravdivé, protože bylo zveřejněno známým účtem a dosud nebylo vyvráceno." = "C",
        "Připustit, že v některých případech není možné s jistotou určit, zda je video pravé nebo generované AI, a proto je nutné k jeho obsahu přistupovat s kritickým odstupem a hledat další ověřené informace." = "D"
      ))
    )
    
  })

    
  sd_server(db = db,
            language = "cs"
  )

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
