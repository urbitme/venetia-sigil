const { sein, patp, patp2hex, hex2patp, patq2hex, isValidPatq } = require('urbit-ob')
const checkWord = require('check-word')
const _cliProgress = require('cli-progress')
const fs = require('fs')
const inquirer = require('inquirer')
const words = checkWord('en')

const pre = `
dozmarbinwansamlitsighidfidlissogdirwacsabwissib\
rigsoldopmodfoglidhopdardorlorhodfolrintogsilmir\
holpaslacrovlivdalsatlibtabhanticpidtorbolfosdot\
losdilforpilramtirwintadbicdifrocwidbisdasmidlop\
rilnardapmolsanlocnovsitnidtipsicropwitnatpanmin\
ritpodmottamtolsavposnapnopsomfinfonbanmorworsip\
ronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig\
sivtagpadsaldivdactansidfabtarmonranniswolmispal\
lasdismaprabtobrollatlonnodnavfignomnibpagsopral\
bilhaddocridmocpacravripfaltodtiltinhapmicfanpat\
taclabmogsimsonpinlomrictapfirhasbosbatpochactid\
havsaplindibhosdabbitbarracparloddosbortochilmac\
tomdigfilfasmithobharmighinradmashalraglagfadtop\
mophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc\
nimlarfitwalrapsarnalmoslandondanladdovrivbacpol\
laptalpitnambonrostonfodponsovnocsorlavmatmipfip\
`
const pre_list = pre.match(/(.{1,3})/g)

const suf = `
zodnecbudwessevpersutletfulpensytdurwepserwylsun\
rypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex\
lunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb\
pyldulhetmevruttylwydtepbesdexsefwycburderneppur\
rysrebdennutsubpetrulsynregtydsupsemwynrecmegnet\
secmulnymtevwebsummutnyxrextebfushepbenmuswyxsym\
selrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel\
syptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed\
lytdusnebrumtynseglyxpunresredfunrevrefmectedrus\
bexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer\
tenlusnussyltecmexpubrymtucfyllepdebbermughuttun\
bylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl\
wedducfurfexnulluclennerlexrupnedlecrydlydfenwel\
nydhusrelrudneshesfetdesretdunlernyrsebhulryllud\
remlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun\
lyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes\
`
const suf_list = suf.match(/(.{1,3})/g)

/*****************
 * Syllable shapes
 *****************/

const marbles = ["bar","bol","dar","dib","dol","dop","doz","fol","fop","hap","har","hol","hop","lar","lib","lig",
                 "lop","mar","mol","mop","nar","nib","nol","nop","par","pol","rib","rig","rol","rop","sar","sib",
                 "sig","sol","sop","tap","tar","tol","top","wol","bet","dem","det","dul","fed","fet","ful","het",
                 "hul","led","let","lun","med","met","mul","mun","ned","nem","net","nul","ped","pem","pet","pun",
                 "red","rem","ret","rul","sed","sem","set","sul","sun","ted","tem","tul","wed","wet","zod"]
const squares = ["bin","das","din","div","dov","fas","fin","has","hin","las","lin","liv","lom","mas","min","nom",
                 "nov","pas","pin","rin","riv","rov","sip","siv","som","sov","tas","tin","tom","win","ben","deb",
                 "deg","den","dut","feb","fen","heb","hut","leb","leg","len","lut","meb","meg","mut","neb","nut",
                 "peg","pen","put","reb","reg","ren","rut","seb","seg","sen","sut","teb","teg","ten","web","weg",
                 "wen"]

const pills = {
  back: ["bal","bit","dal","fit","lit","loc","mal","mit","moc","nal","pal","pit","poc","rac","ral","rit",
         "roc", "sal","sit","tal","toc","def","lyd","nyd","ref","ryd","sef","syd","tyd","wyd"],
  front: ["doc","fal","hal","soc","wal","wit","bel","byr","del","dyr","fel","fyr","lyr","mel","myr","nel",
          "nyr","pel","rel","sel","syr","tel","tyr","tyv","wel"]
}

const slugs = {
  N: ["hob","nim","sim","tim","tob","fun","tun"],
  S: ["der","dur","per","ser","ter","wer"],
  W: ["bil","bor","bot","dan","dav","dig","dil","dor","dot","fam","fan","fig","fil","fip","fod","for",
      "fot","hil","hoc","hod","lag","lan","lod","lor","mag","mig","mil","mod","mor","mot","nam","nil",
      "noc","nod","nor","pag","pil","pod","rag","ram","ran","ril","rip","sam","sav","sil","sor","tag",
      "tam","til","tod","tor","wor"],
  E: ["ber","bex","byt","dev","dex","dun","dyt","fer","fex","hex","ler","lev","lex","lug","lup","lyt",
      "mer","mev","mex","mug","ner","nev","nex","nup","nyt","pex","rev","rex","run","rup","ryt","sev",
      "sug","sup","syt","tev","tex","tug","wex","wyt"]
}

const wedges = {
  NW: ["bep","bon","dep","des","dis","don","fes","fog","fon","hes","lab","lad","lav","lep","lon","map",
       "mep","mip","mis","mog","mon","nap","nes","nis","pes","pon","rab","rep","res","ris","ron","sep",
       "sog","son","tep","tes","tip","tog","ton","wep","wes","wic"],
  NE: ["bec","bes","bud","bus","dec","duc","dus","dyl","dys","fep","fus","hec","hep","hus","lec","lis",
       "luc","lud","lur","lus","lys","mec","mes","mic","mud","mur","mus","nec","nep","nub","num","nus",
       "nux","nyl","nys","pec","pic","pub","pyl","rad","rec","ric","ruc","rud","rus","rux","ryc","rys",
       "sec","sub","sud","sur","tec","tuc","tud","tus","tux","tyc","tyl","wyc","wyl"],
  SW: ["bac","ban","bat","bic","bid","bis","bos","dab","dac","dap","dat","dif","dir","dos","fab","fid",
       "fir","hab","hac","hat","hav","hid","hos","lac","lap","lat","lid","los","mac","mat","mid","mir",
       "mos","nac","nat","nav","nid","nos","pac","pad","pan","pat","pid","pos","rap","rav","rid","ros",
       "ryl","sab","san","sap","sat","sic","sid","tab","tac","tad","tan","tic","tid","tir","wac","wan",
       "wat","wid","wis"],
  SE: ["bur","byl","byn","dux","dyn","fad","fos","fur","fyl","fyn","had","han","lux","lyn","lyx","myl",
       "myn","nym","nyx","pur","pyx","rum","ryg","rym","ryn","ryp","ryx","sum","syl","sym","syn","syp",
       "syx","tyn","typ","wyn","wyx"]
}

/***********************
 * Syllable minor features (lines, dots, etc.)
 * (Very Incomplete)
 ***********************/

const cirq50 = {
  NW: ["bor","mis","ton","bep","dep","fen","meg","myn","nem","nyl","syx","wel"],
  NE: ["dov","fal","mic","pic","sal","tan","bes","deg","rus","syn"],
  SW: ["lav","nav","pal","pat","sov","nep","rem","ryl","seb"],
  SE: ["hid","lat","lit","lon","nal","nov","ris","rov","sor","byn","deb","nyr","sem","sum","syp","wen","wyn"]
}

const vlip50 = {
  NW: [],
  NE: ["hal",],
  SW: ["lac","nid"],
  SE: []
}

const diag_lines = {
  back: ["bac","bar","bor","bos","dis","dos"],
  front: ["dac","dil","dov"],
  back_seg: ["bil","dap","dig","dil","dir"],
  front_seg: ["dab"],
  imcom_back: ["dal"],
  imcom_front: ["bit"]
}

const strate = {
  centv: ["dac","dan","das","dig","div","doc","fam","fan","fig","fil","hal","har","hil","lan","liv","lor",
          "mar","mig","mip","nap","noc","pas","poc","ram","ran","rap","rid","rin","riv","sam","sar","sim",
          "sip","siv","soc","tas","tid","tip","wal","win","wit","dex","fer","let","lud","lur","nes","net",
          "nym","per","rud","run","rup","sen","sep","sub"],
  centh: ["bid","bis","bol","dac","dav","dib","dol","dop","dor","fam","fil","fol","hil","hob","hol","hop",
          "lac","lar","lib","lis","loc","lop","mag","mid","mil","moc","mol","nam","nib","nid","nil","nol",
          "rib","roc","rol","sap","sid","sol","tam","tap","tol","wac","wis","wol","byt","duc","dyt","luc",
          "lud","lup","mep","mur","nub","nux","nyt","rel","ret","rex","ruc","rud","rux","sud","sup","tuc",
          "tud","tug","tux","wet"],
  cross: ["dac","fam","hil","lud","rud"]
}

const black_white = {
  NW: ["bal","dal"],
  NE: [],
  SW: ["dar","doz"],
  SE: ["dif"]
}

const eyes = {
  small: {
    one: ["bac","bal","bos","bot","dar","dav","dop","dot"],
    two: ["dis"]
  },
  ring: {
    small: ["bar","dan","dig","dir","doc"],
    large: ["bol"]
  },
  thick_ring: {
    medium: ["dos"],
    large: ["dop"]
  },
  target: ["bil","das"],
  medium: ["bit","dap"],
  large: ["dif"]
}

const solid = ["bin","fas","han","has","mac","mas","nar","pon","rac","tor","wan","dev","dur","ful","lev","lug",
               "lun","mun","ned","nup","pex","ryg","sun","tev","wyc","zod"]
const moons = ["bac","dab"]
const dudes = ["nam","nod","rig","sig","tod","mur","sed","sug"]

/*********************
 * Detect sigil shapes
 *********************/

const isPre = (syl) => pre_list.includes(syl)
const isSuf = (syl) => suf_list.includes(syl)
const pres = (syls) => syls.filter(isPre)
const sufs = (syls) => syls.filter(isSuf)

const nw = (planet) => planet.substring(1,4)
const ne = (planet) => planet.substring(4,7)
const sw = (planet) => planet.substring(8,11)
const se = (planet) => planet.substring(11)

// Note: the `!Array.isArray(x)` branches are for Sets created below
// They represent logical OR groups

function shapeMatches(shape, planet) {
  const quads = [nw, ne, sw, se]
  return quads.map((quad,i) => {
    if (shape[i] == null) {
      return true
    } else if (!Array.isArray(shape[i])) {
      return anyTrue(Array.from(shape[i]).map(s => s.includes(quad(planet))))
    } else {
      return shape[i].includes(quad(planet))
    }
  })
}

// Probabilites not perfect in this older code
// Does not consider ~dozzod-* fake planets

function shapeProb(shape) {
  const xes = [pres, sufs, pres, sufs]
  return xes.map((xt, i) => {
    if (shape[i] == null) {
      return 256
    } else if (!Array.isArray(shape[i])) {
      return Array.from(shape[i]).map(s => xt(s).length).reduce((x,y) => x + y)
    } else {
      return xt(shape[i]).length
    }
  }).reduce((x, y) => x * y)
}

// Very inefficient for shapes with many instances

function shapeAllInstances(shape) {
  const flats = shape.map((st, i) => {
    if (st == null) {
      return pre_list.concat(suf_list)
    } else if (!Array.isArray(st)) {
      return Array.from(st).reduce((x,y) => x.concat(y))
    } else {
      return st
    }
  })

  let planets = []
  for (let nw of pres(flats[0])) {
    for (let ne of sufs(flats[1])) {
      for (let sw of pres(flats[2])) {
        for (let se of sufs(flats[3])) {
          planets.push('~' + nw + ne + '-' + sw + se)
        }
      }
    }
  }
  return planets
}

// Using `Set` is just a hack for logical OR
// See above: `!Array.isArray(x)` is for these Sets

const shapes = {
  //ptero:      [["tar","tir", "tor"] , ["red","ret","rud","rut","ryd","ryl","ryt"], ["dac"], ["tyl"]],
  //dangel:     [ wedges.NE           , wedges.NW      , slugs.S       , slugs.S]     ,
  cherubs:    [ wedges.SE           , wedges.SW      , wedges.SE     , wedges.SW]   ,
  angel:      [ slugs.N             , slugs.N        , wedges.SE     , wedges.SW]   ,
  suntomb:    [ wedges.SE           , slugs.N        , wedges.SE     , slugs.N]     ,
  loop50:     [ cirq50.NW           , cirq50.NE      , cirq50.SW     , strate.cross],
  fourdudes:  [ dudes               , dudes          , dudes         , dudes]       ,
  loop51:     [ cirq50.NW           , strate.cross   , cirq50.SW     , cirq50.SE]   ,
  loop52:     [ cirq50.NW           , cirq50.NE      , strate.cross  , cirq50.SE]   ,
  loop53:     [ strate.cross        , cirq50.NE      , cirq50.SW     , cirq50.SE]   ,
  butterfly:  [ wedges.NE           , wedges.NW      , wedges.SE     , wedges.SW]   ,
  star:       [ wedges.SE           , wedges.SW      , wedges.NE     , wedges.NW]   ,
  hair:       [ cirq50.NW           , cirq50.NE      , cirq50.SE     , cirq50.SW]   ,
  cq50:       [ cirq50.NW           , cirq50.NE      , cirq50.SW     , cirq50.SE]   ,
  tipis:      [ pills.front         , pills.back     , pills.front   , pills.back]  ,
  valleys:    [ wedges.NE           , wedges.NW      , wedges.NE     , wedges.NW]   ,
  downu:      [ cirq50.NW           , cirq50.NE      , strate.centv  , strate.centv],
  rightu:     [ cirq50.NW           , strate.centh   , cirq50.SW     , strate.centh],
  pillfielda: [ pills.front         , pills.front    , pills.front   , pills.front] ,
  xcurve2:    [ cirq50.SW           , strate.centh   , strate.centh  , cirq50.NE]   ,
  ball:       [ pills.front         , pills.back     , pills.back    , pills.front] ,
  fourleaf:   [ pills.back          , pills.front    , pills.front   , pills.back]  ,
  upu:        [ strate.centv        , strate.centv   , cirq50.SW     , cirq50.SE]   ,
  xcurve1:    [ strate.centh        , cirq50.SE      , cirq50.NW     , strate.centh],
  foursolid:  [ solid               , solid          , solid         , solid]       ,
  pillfieldb: [ pills.back          , pills.back     , pills.back    , pills.back]  ,
  heart:      [ slugs.N             , slugs.N        ,
    new Set(  [ wedges.SW           , pills.back])   ,
    new Set(  [ wedges.SE           , pills.front])] ,
  leftu:      [ strate.centh        , cirq50.NE      , strate.centh  , cirq50.SE]   ,
  solidude:   [ new Set([ solid     , dudes])        ,
    new Set(  [ solid               , dudes])        ,
    new Set(  [ solid               , dudes])        ,
    new Set(  [ solid               , dudes])]       ,
  pillstar:   [ new Set([ wedges.SE , pills.front])  ,
    new Set(  [ wedges.SW           , pills.back])   ,
    new Set(  [ wedges.NE           , pills.back])   ,
    new Set(  [ wedges.NW           , pills.front])] ,
  diangar:    [ wedges.SW           , pills.front    ,
    new Set(  [ squares             , wedges.SW])    , wedges.SW]   ,
  funnels:    [ pills.back          , pills.front    , pills.back    , pills.front] ,
  pillpiea:   [ wedges.NW           , pills.back     , pills.back    , wedges.SE]   ,
  scurve2:    [ cirq50.NW           , strate.centh   , cirq50.SE     , null]        ,
  zcurve1:    [ strate.centh        , cirq50.NE      , null          , cirq50.SW]   ,
  zcurve2:    [ cirq50.NE           , null           , cirq50.SW     , strate.centh],
  diangal:    [ pills.back          , wedges.SE      , wedges.SE     ,
    new Set(  [ squares             , wedges.SE])]   ,
  centv:      [ strate.centv        , strate.centv   , strate.centv  , strate.centv],
  gift:       [ pills.back          , pills.front    , squares       , squares]     ,
  pillpieb:   [ pills.front         , wedges.NE      , wedges.SW     , pills.front] ,
  scurve1:    [ null                , cirq50.NW      , strate.centh  , cirq50.SE]   ,
  moth:       [ pills.back          , pills.front    , slugs.W       , slugs.E]     ,
  grin:       [ pills.back          , pills.front    , wedges.SW     , wedges.SE]   ,
  leafr:      [ wedges.NW           , squares        , squares       , wedges.SE]   ,
  rightbowl:  [ null                , wedges.NW      , null          , wedges.SW]   ,
  foursquare: [ squares             , squares        , squares       , squares]     ,
  centh:      [ strate.centh        , strate.centh   , strate.centh  , strate.centh],
  leftbowl:   [ wedges.NE           , null           , wedges.SE     , null]        ,
  rattler:    [ wedges.NW           , marbles        , marbles       , wedges.SE]   ,
  decol:      [ marbles             , squares        , squares       , marbles]     ,
  decor:      [ squares             , marbles        , marbles       , squares]     ,
  fourmarble: [ marbles             , marbles        , marbles       , marbles]     ,
  rangel:     [ wedges.SW           , slugs.E        , wedges.NW     , slugs.E]     ,
  pie:        [ wedges.NW           , wedges.NE      , wedges.SW     , wedges.SE]   ,
  smile:      [ marbles             , marbles        , wedges.SW     , wedges.SE]   ,
  legs:       [ null                , null           , pills.front   , pills.back]  ,
  leafl:      [ squares             , wedges.NE      , wedges.SW     , squares]     ,
  heartright: [ new Set([wedges.NW  , pills.front])  , slugs.E       ,
    new Set(  [ wedges.SW           , pills.back])   , slugs.E]      ,
  capsules:   [ slugs.W             , slugs.E        , slugs.W       , slugs.E]     ,
  langel:     [ slugs.W             , wedges.SE      , slugs.W       , wedges.NE]   ,
  vlipleft:   [ strate.centv        , null           , vlip50.SW     , null]        ,
  rattlel:    [ marbles             , wedges.NE      , wedges.SW     , marbles]     ,
  snowa:      [ wedges.NW           ,
    new Set(  [ marbles             , pills.front    , squares])     ,
    new Set(  [ marbles             , pills.front    , squares])     , wedges.SE]    ,
  circle:     [ new Set([ wedges.NW , pills.front])  ,
    new Set(  [ wedges.NE           , pills.back])   ,
    new Set(  [ wedges.SW           , pills.back])   ,
    new Set(  [ wedges.SE           , pills.front])] ,
  heartleft:  [ slugs.W             ,
    new Set(  [ wedges.NE           , pills.back])   , slugs.W       ,
    new Set(  [ wedges.SE           , pills.front])] ,
  fish:       [ wedges.SW           , wedges.NE      ,
    new Set(  [ slugs.W             , wedges.NW])    , wedges.SE]    ,
  bee:        [ pills.back          , pills.front    ,
    new Set(  [ slugs.W             , wedges.NW      , wedges.SW     , squares])    ,
    new Set(  [ slugs.E             , wedges.NE      , wedges.SE     , squares])],
  snowb:      [ new Set([ marbles   , pills.back     , squares])     , wedges.NE     , 
                wedges.SW           ,
    new Set(  [ marbles             , pills.back     , squares])]
}

// incomplete
const cool_words = ["walnut","master","docter","doctyr","topfun","winter","former","hodler",
                    "mister","salted","possum","magnet","wishes","walled","wordes","ponnys",
                    "dishes","walrus","botnet","ladder","parsec","donnut","windex","wanted",
                    "balled","ballet","ramped","banter","banned","bonnet","barber","barrel",
                    "barbes","barfed","barter","bathes","tonnes","molten","parsed","batter",
                    "dissed","minter","salted","litter","bidder","binder","pasted","mitten",
                    "martyr","bitten","bitter","bolder","tanned","bolted","banner","fasten",
                    "bossen","bosser","wallet","normes","dander","dactyl","sorted","signer",
                    "tornep","sampel","mastyr","hodlur","hodlyr","winner","filtyr","filter",
                    "hatred","parser","matryx","winsum","battel","pollux","ballen","timber",
                    "biches","misted","lattes","waltyr","larper","morsul"]

function objectMap(object, mapFn) {
  return Object.keys(object).reduce(function(result, key) {
    result[key] = mapFn(object[key])
    return result
  }, {})
}

const preds = objectMap(shapes, s => p => allTrue(shapeMatches(s,p)))
const preds_three = objectMap(shapes, s => p => threeTrue(shapeMatches(s,p)))

function countTrues(predList) {
  let count = 0
  for (let b of predList) {
    if (b) { count++ }
  }
  return count
}

function allTrue(predList) {
  return countTrues(predList) == predList.length
}

function threeTrue(predList) {
  return countTrues(predList) == 3
}

function anyTrue(predList) {
  return countTrues(predList) > 0
}

const name_preds = {
  doublename: pl => nw(pl) == sw(pl) && ne(pl) == se(pl),
  doublepre: pl => nw(pl) == sw(pl),
  doublesuf: pl => ne(pl) == se(pl),
  coolnames: pl => cool_words.includes(nw(pl)+ne(pl)) || cool_words.includes(sw(pl)+se(pl)) 
}

function newProgress(title) {
  return new _cliProgress.Bar({
    format: '| {percentage}% | ETA: {eta}s | {value} | Found: {found}\t ' + title
  }, _cliProgress.Presets.shades_classic)
}

function allPlanets(star) {
  const hex = patq2hex(star)
  let planets = []

  for (let i = 1; i <= 0xFFFF; i++) {
    const s = i.toString(16).padStart(4, '0') + hex
    const p = hex2patp(s)
    planets.push(p)
  }

  return planets.sort()
}

function allStars() {
  let stars = []
  for (let i=256; i<=65535; i++) {
    stars.push(patp(i))
  }
  return stars
}

function searchAndSaveFile(planets, folder, filename, predFunc) {
  let progress = newProgress(filename + '\t')
  let index = 0
  let found = 0
  let matches = []

  progress.start(planets.length, 0)

  for (let planet of planets) {
    if (predFunc(planet)) {
      matches.push(planet)
      found++
    }
    progress.update(++index, { found })
  }

  writeListFile(folder, filename, matches)

  progress.stop()
}

function writeListFile(folder, filename, lines) {
  if (lines.length == 0) { return }

  if (!fs.existsSync(__dirname + '/' + folder)) {
    fs.mkdirSync(__dirname + '/' + folder)
  }

  let file_contents = lines.join('\n') + '\n'

  if (fs.existsSync(__dirname + '/' + folder + '/' + filename + '.txt')) {
    let existing_contents = fs.readFileSync(__dirname + '/' + folder + '/' + filename + '.txt', 'utf8')

    if (existing_contents == file_contents) {
      return
    }
  }

  fs.writeFile(__dirname + '/' + folder + '/' + filename + '.txt', file_contents, function(err) {
    if (err) { return console.log(err) }
  })
}

function forEachStar(stars, processFunc) {
  for (let st of stars) {
    star = st.length == 7 ? st : '~' + st
    console.log('\x1b[36m\x1b[1m%s\x1b[0m', star)
    let star_alpha = star.replace('~','')
    let planets = allPlanets(star)
    processFunc(planets, star_alpha)
  }
}

function allPlanetsList(planets, star_alpha) {
  searchAndSaveFile(planets, star_alpha, 'all', (planet) => true)
}

function rareSigilsList(planets, star_alpha) {
  for (let shape of Object.keys(shapes)) {
    searchAndSaveFile(planets, star_alpha, shape, preds[shape])
  }
}

const coolNamesList = (planets, star_alpha) => {
  for (let pred of Object.keys(name_preds)) {
    searchAndSaveFile(planets, star_alpha, pred, name_preds[pred])
  }
}

function threeSigilsList(planets, star_alpha) {
  const probs = objectMap(shapes, s => shapeProb(s))
  for (let shape of Object.keys(shapes)) {
    if (probs[shape] < 100000) {
      searchAndSaveFile(planets, star_alpha, 'three_' + shape, preds_three[shape])
    }
  }
}

function containsWordsList(planets, star_alpha) {
  searchAndSaveFile(planets, star_alpha, 'words', (planet) => {
    return planet.replace('~', '').split('-')
           .filter(part => words.check(part)).length
  })
}

function onlyWordsList(planets, star_alpha) {
  searchAndSaveFile(planets, star_alpha, 'words-only', (planet) => {
    return planet.replace('~', '').split('-')
           .filter(part => words.check(part)).length === 2
  })
}

function rarePlanetLists() {
  const probs = objectMap(shapes, s => shapeProb(s))
  for (let shape of Object.keys(shapes)) {
    if (probs[shape] < 512) {
      let planets = shapeAllInstances(shapes[shape])
      writeListFile('rares', shape, planets)
      let star_planets = planets.map(s => sein(s) + ' ' + s).sort()
      writeListFile('rares_stars', shape, star_planets)
    }
  }
}

function printAllStars() {
  for (let star of allStars()) {
    console.log(star)
  }
}

function printSigilCounts() {
  const probs = objectMap(shapes, s => shapeProb(s))
  for (let name of Object.keys(probs)) {
    console.log((" ".repeat(12) + probs[name]).slice(-12) + '  ' + (probs[name] * 100 / 4294902016).toFixed('7') + '%  ' + name)
  }
}

function printGlyphCounts() {
  console.log("Marble count: " + marbles.length)
  console.log("Square count: " + squares.length)
  console.log("B Pill count: " + pills.back.length)
  console.log("F Pill count: " + pills.front.length)
  console.log("Slug N count: " + slugs.N.length)
  console.log("Slug S count: " + slugs.S.length)
  console.log("Slug W count: " + slugs.W.length)
  console.log("Slug E count: " + slugs.E.length)
  console.log("Pie NW count: " + wedges.NW.length)
  console.log("Pie NE count: " + wedges.NE.length)
  console.log("Pie SW count: " + wedges.SW.length)
  console.log("Pie SE count: " + wedges.SE.length)
  console.log(" ")
  console.log("Marble count: " + pres(marbles).length + " " + sufs(marbles).length)
  console.log("Square count: " + pres(squares).length + " " + sufs(squares).length)
  console.log("B Pill count: " + pres(pills.back).length + " " + sufs(pills.back).length)
  console.log("F Pill count: " + pres(pills.front).length + " " + sufs(pills.front).length)
  console.log("Slug N count: " + pres(slugs.N).length + " " + sufs(slugs.N).length)
  console.log("Slug S count: " + pres(slugs.S).length + " " + sufs(slugs.S).length)
  console.log("Slug W count: " + pres(slugs.W).length + " " + sufs(slugs.W).length)
  console.log("Slug E count: " + pres(slugs.E).length + " " + sufs(slugs.E).length)
  console.log("Pie NW count: " + pres(wedges.NW).length + " " + sufs(wedges.NW).length)
  console.log("Pie NE count: " + pres(wedges.NE).length + " " + sufs(wedges.NE).length)
  console.log("Pie SW count: " + pres(wedges.SW).length + " " + sufs(wedges.SW).length)
  console.log("Pie SE count: " + pres(wedges.SE).length + " " + sufs(wedges.SE).length)
}

function printInvalidPlanets() {
  let progress = newProgress('All\t')
  let index = 0
  let found = 0

  let allPlanets = []
  progress.start(2**32, 0)

  for (let sw of pre_list) {
    for (let se of suf_list) {
      allPlanets.push('~dozzod-' + sw + se)
      progress.update(++index, { index })
    }
  }
  progress.stop()

  console.log(allPlanets.length)
}

function mainMenu() {
  inquirer.prompt([
    {
      type: 'list',
      name: 'section',
      message: 'Main Menu',
      default: 'info',
      choices: [
        {value: 'info', name: 'General Information'},
        {value: 'stars', name: 'Star Actions'},
      ]
    }
  ]).then(answers => {
    if (answers.section == 'stars') {
      starSelect()
    } else {
      sigilMenu()
    }
  })
}

function sigilMenu() {
  inquirer.prompt([
    {
      type: 'list',
      name: 'action',
      message: 'Sigil Information',
      default: 'rares',
      choices: [
        {value: 'glyph', name: 'Sigil Glyph Counts'},
        {value: 'fakes', name: 'Planets which do not exist'},
        {value: 'rares', name: 'Rare Sigil Counts'},
        {value: 'all4r', name: 'Rare Planet Lists'},
        {value: 'angel', name: 'Angel Planet List'},
        {value: 'stars', name: 'All Star Names'},
      ]
    }
  ]).then(answers => {
    const actions = {
      glyph: printGlyphCounts,
      fakes: printInvalidPlanets,
      rares: printSigilCounts,
      stars: printAllStars,
      all4r: rarePlanetLists
    }

    actions[answers.action]()
  })
}

function starSelect() {
  inquirer.prompt([
    {
      type: 'list',
      name: 'star',
      message: 'Which star(s)?',
      default: 'rondev',
      choices: [
        {value: 'rondev', name: '~rondev'},
        {value: 'enter', name: 'Enter star name'},
      ]
    }
  ]).then(answers => {
    const stars = {
      rondev: ["~rondev"]
    }

    if (answers.star == 'enter') {
      starEntry()
    } else {
      starMenu(stars[answers.star])
    }
  })
}

function starEntry() {
  inquirer.prompt([
    {
      type: 'string',
      message: 'Star name',
      name: 'name',
      validate: function (input) {
        let star = input.length == 6 ? '~' + input : input
        return (isValidPatq(star) && star.length === 7) ? true : 'Invalid star name'
      }
    }
  ]).then(answers => {
    starMenu([answers.name])
  })
}

function starMenu(stars, batch) {
  inquirer.prompt([
    {
      type: 'list',
      name: 'action',
      message: 'Star Action',
      default: 'shapes',
      choices: [
        {value: 'all', name: 'All possible planets'},
        {value: 'shapes', name: 'Planets with interesting sigils'},
        {value: 'names', name: 'Planets with interesting names'},
        {value: 'shapes_three', name: 'Planets with less rare sigils (3 of 4)'},
        {value: 'words', name: 'Planets with English words in their names'},
        {value: 'lexical', name: 'Planets only containing English words'}
      ]
    }
  ]).then(answers => {
    const list_types = {
      all: allPlanetsList,
      shapes: rareSigilsList,
      names: coolNamesList,
      shapes_three: threeSigilsList,
      words: containsWordsList,
      lexical: onlyWordsList
    }

    forEachStar(stars, list_types[answers.action])
  })
}

mainMenu()

