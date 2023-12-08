{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
module Text.Pandoc.Citeproc.Data
  (biblatexStringMap)
where
import Data.FileEmbed
import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Citeproc.Util (toIETF)
import Text.Collate.Lang (Lang(..), parseLang)

import qualified GHC.IO.Unsafe
import qualified Data.ByteString.Unsafe

biblatexLocalizations :: [(FilePath, ByteString)]
biblatexLocalizations =
      [("UKenglish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("USenglish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("american.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("australian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("austrian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 30)
             "january|J\\\"anner|J\\\"an\\adddot\n"#)),
       ("brazil.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("brazilian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 7903)
             "bibliography|Bibliografia|Bibliografia\n\
             \references|Refer\\^encias|Refer\\^encias\n\
             \shorthands|Lista de abreviaturas|Abreviaturas\n\
             \editor|editor|ed\\adddot\n\
             \editors|editores|ed\\adddot\n\
             \compiler|compilador|comp\\adddot\n\
             \compilers|compiladores|comp\\adddot\n\
             \redactor|redator|red\\adddot\n\
             \redactors|redatores|red\\adddot\n\
             \reviser|revisor|rev\\adddot\n\
             \revisers|revisores|rev\\adddot\n\
             \founder|fundador|fund\\adddot\n\
             \founders|fundadores|fund\\adddot\n\
             \continuator|continuador|cont\\adddot\n\
             \continuators|continuadores|cont\\adddot\n\
             \collaborator|colaborador|colab\\adddot\n\
             \collaborators|colaboradores|colab\\adddot\n\
             \translator|tradutor|trad\\adddot\n\
             \translators|tradutores|trad\\adddot\n\
             \commentator|coment\\'ario|coment\\adddot\n\
             \commentators|coment\\'ario|coment\\adddot\n\
             \annotator|notas|notas\n\
             \annotators|notas|notas\n\
             \commentary|coment\\'ario|coment\\adddot\n\
             \annotations|notas|notas\n\
             \foreword|pref\\'acio|pref\\adddot\n\
             \afterword|posf\\'acio|posf\\adddot\n\
             \organizer|organizador|org\\adddot\n\
             \organizers|organizadores|org\\adddot\n\
             \byauthor|por|por\n\
             \byeditor|editado por|ed\\adddotspace por\n\
             \bycompiler|compilado por|comp\\adddotspace por\n\
             \byredactor|redigido por|red\\adddotspace por\n\
             \byreviser|revisado por|rev\\adddotspace por\n\
             \byreviewer|resenhado por|res\\adddotspace por\n\
             \byfounder|fundado por|fund\\adddotspace por\n\
             \bycontinuator|continuado por|cont\\adddotspace por\n\
             \bytranslator|traduzido \\lbx@lfromlang\\ por|trad\\adddot\\ \\lbx@sfromlang\\ por\n\
             \bycommentator|comentado por|coment\\adddot\\ por\n\
             \byannotator|anotado por|anot\\adddot\\ por\n\
             \byorganizer|organizado por|org\\adddotspace por\n\
             \withcommentator|com coment\\'ario de|com coment\\adddot\\ de\n\
             \withannotator|com notas de|com notas de\n\
             \withforeword|com pref\\'acio de|com pref\\adddot\\ de\n\
             \withafterword|com posf\\'acio de|com posf\\adddot\\ de\n\
             \and|e|e\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \volume|volume|vol\\adddot\n\
             \volumes|volumes|vol\\adddot\n\
             \involumes|em|em\n\
             \jourvol|volume|v\\adddot\n\
             \jourser|s\\'erie|s\\'er\\adddot\n\
             \book|livro|livro\n\
             \part|parte|parte\n\
             \issue|n\\'umero|n\\'um\\adddot\n\
             \newseries|nova s\\'erie|nova s\\'er\\adddot\n\
             \oldseries|s\\'erie antiga|s\\'er\\adddot\\ antiga\n\
             \reprint|reimpress\\~ao|reimpr\\adddot\n\
             \reprintof|reimpress\\~ao de|reimpr\\adddotspace de\n\
             \reprintas|reimpresso como|reimpr\\adddotspace como\n\
             \reprintfrom|reimpresso de|reimpr\\adddotspace de\n\
             \translationas|traduzido como|trad\\adddotspace como\n\
             \translationfrom|traduzido do|trad\\adddotspace do\n\
             \reviewof|resenha de|res\\adddotspace de\n\
             \origpubas|originalmente publicado como|orig\\adddotspace pub\\adddotspace como\n\
             \origpubin|originalmente publicado em|orig\\adddotspace pub\\adddotspace em\n\
             \astitle|como|como\n\
             \bypublisher|por|por\n\
             \nodate|sem\\space data|s\\adddot d\\adddot\n\
             \page|p\\'agina|p\\adddot\n\
             \pages|p\\'aginas|pp\\adddot\n\
             \column|coluna|col\\adddot\n\
             \columns|colunas|col\\adddot\n\
             \line|linha|l\\adddot\n\
             \lines|linhas|ll\\adddot\n\
             \verse|verso|v\\adddot\n\
             \verses|versos|vv\\adddot\n\
             \paragraph|par\\'agrafo|par\\adddot\n\
             \paragraphs|par\\'agrafos|par\\adddot\n\
             \pagetotal|p\\'agina|p\\adddot\n\
             \pagetotals|p\\'aginas|pp\\adddot\n\
             \columntotal|coluna|col\\adddot\n\
             \columntotals|colunas|col\\adddot\n\
             \linetotal|linha|l\\adddot\n\
             \linetotals|linhas|ll\\adddot\n\
             \versetotal|verso|v\\adddot\n\
             \versetotals|versos|vv\\adddot\n\
             \paragraphtotal|par\\'agrafo|par\\adddot\n\
             \paragraphtotals|par\\'agrafos|par\\adddot\n\
             \in|em|em\n\
             \inseries|em|em\n\
             \ofseries|de|de\n\
             \number|n\\'umero|n\\adddot\n\
             \chapter|cap\\'\\i tulo|cap\\adddot\n\
             \bathesis|trabalho de conclus\\~ao|trab\\adddotspace de conc\\adddot\n\
             \phdthesis|tese de doutorado|tese de dout\\adddot\n\
             \candthesis||\n\
             \resreport|relat\\'orio de pesquisa|rel\\adddotspace de pesq\\adddot\n\
             \techreport|relat\\'orio t\\'ecnico|rel\\adddotspace t\\'ecn\\adddot\n\
             \software|software|software\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|CD de \\'audio|CD de \\'audio\n\
             \version|vers\\~ao|vers\\~ao\n\
             \urlfrom|dispon\\'ivel em|disp\\adddotspace em\n\
             \urlseen|acesso em|acesso em\n\
             \submitted|submetido|submetido\n\
             \inpress|no prelo|no prelo\n\
             \prepublished|pr\\'e-publicado|pr\\'e-publicado\n\
             \citedas|doravante citado como|doravante citado como\n\
             \thiscite|especialmente|esp\\adddot\n\
             \seenote|ver nota|ver nota\n\
             \quotedin|apud|apud\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|passim\n\
             \see|ver|ver\n\
             \seealso|ver tamb\\'em|ver tamb\\'em\n\
             \backrefpage|ver p\\'agina|ver p\\adddot\n\
             \backrefpages|ver p\\'aginas|ver pp\\adddot\n\
             \january|janeiro|jan\\adddot\n\
             \february|fevereiro|fev\\adddot\n\
             \april|abril|abr\\adddot\n\
             \may|maio|mai\\adddot\n\
             \june|junho|jun\\adddot\n\
             \july|julho|jul\\adddot\n\
             \august|agosto|ago\\adddot\n\
             \september|setembro|set\\adddot\n\
             \october|outubro|out\\adddot\n\
             \november|novembro|nov\\adddot\n\
             \december|dezembro|dez\\adddot\n\
             \langamerican|ingl\\^es|ingl\\^es\n\
             \langbulgarian|b\\'ulgaro|b\\'ulgaro\n\
             \langcatalan|catal\\~ao|catal\\~ao\n\
             \langcroatian|croata|croata\n\
             \langczech|tcheco|tcheco\n\
             \langdanish|dinamarqu\\^es|dinamarqu\\^es\n\
             \langenglish|ingl\\^es|ingl\\^es\n\
             \langfinnish|finland\\^es|finland\\^es\n\
             \langestonian|estoniano|estoniano\n\
             \langfrench|franc\\^es|franc\\^es\n\
             \langgalician|galego|galego\n\
             \langgerman|alem\\~ao|alem\\~ao\n\
             \langgreek|grego|grego\n\
             \langhungarian|h\\'ungaro|h\\'ungaro\n\
             \langitalian|italiano|italiano\n\
             \langjapanese|japon\\^es|japon\\^es\n\
             \langlatin|latim|latim\n\
             \langlatvian|let\\~ao|let\\~ao\n\
             \langlithuanian|lituano|lituano\n\
             \langnorwegian|noruegu\\^es|noruegu\\^es\n\
             \langpolish|polon\\^es|polon\\^es\n\
             \langrussian|russo|russo\n\
             \langserbian|s\\'ervio|s\\'ervio\n\
             \langslovak|eslovaco|eslovaco\n\
             \langslovene|esloveno|esloveno\n\
             \langspanish|espanhol|espanhol\n\
             \langswedish|sueco|sueco\n\
             \langturkish|turco|turco\n\
             \langukrainian|ucraniano|ucraniano\n\
             \fromamerican|do ingl\\^es|do ingl\\^es\n\
             \frombulgarian|do b\\'ulgaro|do b\\'ulgaro\n\
             \fromcatalan|do catal\\~ao|do catal\\~ao\n\
             \fromcroatian|do croata|do croata\n\
             \fromczech|do tcheco|do tcheco\n\
             \fromdanish|do dinamarqu\\^es|do dinamarqu\\^es\n\
             \fromenglish|do ingl\\^es|do ingl\\^es\n\
             \fromestonian|do estoniano|do estoniano\n\
             \fromfinnish|do finland\\^es|do finland\\^es\n\
             \fromfrench|do franc\\^es|do franc\\^es\n\
             \fromgalician|do galego|do galego\n\
             \fromgerman|do alem\\~ao|do alem\\~ao\n\
             \fromgreek|do grego|do grego\n\
             \fromhungarian|do h\\'ungaro|do h\\'ungaro\n\
             \fromitalian|do italiano|do italiano\n\
             \fromjapanese|do japon\\^es|do japon\\^es\n\
             \fromlatin|do latim|do latim\n\
             \fromlatvian|do let\\~ao|do let\\~ao\n\
             \fromlithuanian|do lituano|do lituano\n\
             \fromnorwegian|do noruegu\\^es|do noruegu\\^es\n\
             \frompolish|do polon\\^es|do polon\\^es\n\
             \fromrussian|do russo|do russo\n\
             \fromserbian|do s\\'ervio|do s\\'ervio\n\
             \fromslovak|do eslovaco|do eslovaco\n\
             \fromslovene|do esloveno|do esloveno\n\
             \fromspanish|do espanhol|do espanhol\n\
             \fromswedish|do sueco|do sueco\n\
             \fromturkish|do turco|do turco\n\
             \fromukrainian|do ucraniano|do ucraniano\n\
             \countryde|Alemanha|DE\n\
             \countryeu|Uni\\~ao Europeia|EU\n\
             \countryep|Uni\\~ao Europeia|EP\n\
             \countryuk|Reino Unido|GB\n\
             \countryus|Estados Unidos|US\n\
             \patent|patente|pat\\adddot\n\
             \patentde|patente alem\\~a|pat\\adddot\\ alem\\~a\n\
             \patenteu|patente europeia|pat\\adddot\\ europeia\n\
             \patentfr|patente francesa|pat\\adddot\\ francesa\n\
             \patentuk|patente brit\\^anica|pat\\adddot\\ brit\\^anica\n\
             \patentus|patente americana|pat\\adddot\\ americana\n\
             \patreq|pedido de patente|ped\\adddot\\ de pat\\adddot\n\
             \patreqde|pedido de patente alem\\~a|ped\\adddot\\ de pat\\adddot\\ alem\\~a\n\
             \patreqeu|pedido de patente europeia|ped\\adddot\\ de pat\\adddot\\ europeia\n\
             \patreqfr|pedido de patente francesa|ped\\adddot\\ de pat\\adddot\\ francesa\n\
             \patrequk|pedido de patente brit\\^anica|ped\\adddot\\ de pat\\adddot\\ brit\\^anica\n\
             \patrequs|pedido de patente americana|ped\\adddot\\ de pat\\adddot\\ americana\n\
             \file|arquivo|arquivo\n\
             \library|biblioteca|biblioteca\n\
             \abstract|resumo|resumo\n\
             \annotation|notas|notas\n\
             \commonera|Era Comum|EC\n\
             \beforecommonera|antes da Era Comum|AEC\n\
             \annodomini|depois de Cristo|d\\adddot C\\adddot\n\
             \beforechrist|antes de Cristo|a\\adddot C\\adddot\n\
             \circa|circa|ca\\adddot\n\
             \spring|primavera|primavera\n\
             \summer|ver\\~ao|ver\\~ao\n\
             \autumn|outono|outono\n\
             \winter|inverno|inverno\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("british.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 109)
             "organizer|organiser|org\\adddot\n\
             \organizers|organisers|orgs\\adddot\n\
             \byorganizer|organised by|org\\adddotspace by\n"#)),
       ("bulgarian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 1387)
             "bibliography|\\208\\145\\208\\184\\208\\177\\208\\187\\208\\184\\208\\190\\208\\179\\209\\128\\208\\176\\209\\132\\208\\184\\209\\143|\\208\\145\\208\\184\\208\\177\\208\\187\\208\\184\\208\\190\\208\\179\\209\\128\\208\\176\\209\\132\\208\\184\\209\\143\n\
             \references|\\208\\155\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\176|\\208\\155\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\176\n\
             \shorthands|\\208\\161\\208\\191\\208\\184\\209\\129\\209\\138\\208\\186 \\208\\189\\208\\176 \\209\\129\\209\\138\\208\\186\\209\\128\\208\\176\\209\\137\\208\\181\\208\\189\\208\\184\\209\\143\\209\\130\\208\\176|\\208\\161\\209\\138\\208\\186\\209\\128\\208\\176\\209\\137\\208\\181\\208\\189\\208\\184\\209\\143\n\
             \editor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \editors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\208\\184|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \compiler|\\209\\129\\209\\138\\209\\129\\209\\130\\208\\176\\208\\178\\208\\184\\209\\130\\208\\181\\208\\187|\\209\\129\\209\\138\\209\\129\\209\\130\\adddot\n\
             \compilers|\\209\\129\\209\\138\\209\\129\\209\\130\\208\\176\\208\\178\\208\\184\\209\\130\\208\\181\\208\\187\\208\\184|\\209\\129\\209\\138\\209\\129\\209\\130\\adddot\n\
             \redactor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \redactors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\208\\184|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \reviser|\\208\\186\\208\\190\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128|\\208\\186\\208\\190\\209\\128\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \revisers|\\208\\186\\208\\190\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128|\\208\\186\\208\\190\\209\\128\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \founder|\\208\\190\\209\\129\\208\\189\\208\\190\\208\\178\\208\\176\\209\\130\\208\\181\\208\\187|\\208\\190\\209\\129\\208\\189\\adddot\n\
             \founders|\\208\\190\\209\\129\\208\\189\\208\\190\\208\\178\\208\\176\\209\\130\\208\\181\\208\\187\\208\\184|\\208\\190\\209\\129\\208\\189\\adddot\n\
             \continuator|\\208\\191\\209\\128\\208\\190\\208\\180\\209\\138\\208\\187\\208\\182\\208\\184\\209\\130\\208\\181\\208\\187|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \continuators|\\208\\191\\209\\128\\208\\190\\208\\180\\209\\138\\208\\187\\208\\182\\208\\184\\209\\130\\208\\181\\208\\187|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \collaborator|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\208\\189\\208\\184\\208\\186|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\adddot\n\
             \collaborators|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\208\\189\\208\\184\\209\\134\\208\\184|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\adddot\n\
             \translator|\\208\\191\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180\\208\\176\\209\\135|\\208\\191\\209\\128\\208\\181\\208\\178\\adddot\n\
             \translators|\\208\\191\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180\\208\\176\\209\\135\\208\\184|\\208\\191\\209\\128\\208\\181\\208\\178\\adddot\n\
             \commentator|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \commentators|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128\\208\\184|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotator|\\208\\177\\208\\181\\208\\187\\208\\181\\208\\182\\208\\186\\208\\184|\\208\\177\\208\\181\\208\\187\\adddot\n\
             \annotators|\\208\\177\\208\\181\\208\\187\\208\\181\\208\\182\\208\\186\\208\\184|\\208\\177\\208\\181\\208\\187\\adddot\n\
             \commentary|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotations|\\208\\177\\208\\181\\208\\187\\208\\181\\208\\182\\208\\186\\208\\184|\\208\\177\\208\\181\\208\\187\\adddot\n\
             \introduction|\\209\\131\\208\\178\\208\\190\\208\\180\\208\\189\\208\\176 \\209\\129\\209\\130\\208\\176\\209\\130\\208\\184\\209\\143|\\209\\131\\208\\178\\adddotspace \\209\\129\\209\\130\\adddot\n\
             \foreword|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\179\\208\\190\\208\\178\\208\\190\\209\\128|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\179\\adddot\n\
             \afterword|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\208\\190\\208\\178|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\adddot\n"#)),
       ("canadian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("catalan.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 6105)
             "bibliography|Bibliografia|Bibliografia\n\
             \references|Refer\\`encies|Refer\\`encies\n\
             \shorthands|Llista d'abreviatures|Abreviatures\n\
             \editor|editor|ed\\adddot\n\
             \editors|editors|ed\\adddot\n\
             \compiler|compilador|comp\\adddot\n\
             \compilers|compiladors|comp\\adddot\n\
             \redactor|redactor|red\\adddot\n\
             \redactors|redactors|red\\adddot\n\
             \reviser|revisor|rev\\adddot\n\
             \revisers|revisors|rev\\adddot\n\
             \founder|fundador|fund\\adddot\n\
             \founders|fundadors|fund\\adddot\n\
             \continuator|continuador|cont\\adddot\n\
             \continuators|continuadors|cont\\adddot\n\
             \collaborator|co\\l.laborador|co\\l.l\\adddot\n\
             \collaborators|co\\l.laboradors|co\\l.l\\adddot\n\
             \translator|traductor|trad\\adddot\n\
             \translators|traductors|trad\\adddot\n\
             \commentator|comentarista|com\\adddot\n\
             \commentators|comentaristes|com\\adddot\n\
             \annotator|anotador|anot\\adddot\n\
             \annotators|anotadors|anot\\adddot\n\
             \commentary|comentari|com\\adddot\n\
             \annotations|notes|n\\adddot\n\
             \introduction|introducci\\'o|intr\\adddot\n\
             \foreword|pr\\`oleg|pr\\`ol\\adddot\n\
             \editortr|editor i traductor|ed\\adddotspace i trad\\adddot\n\
             \editorstr|editors i traductors|ed\\adddotspace i trad\\adddot\n\
             \editorco|editor i comentarista|ed\\adddotspace i com\\adddot\n\
             \editorsco|editors i comentaristes|ed\\adddotspace i com\\adddot\n\
             \editoran|editor i anotador|ed\\adddotspace i anot\\adddot\n\
             \editorsan|editors i anotadors|ed\\adddotspace i anot\\adddot\n\
             \organizer|organitzador|org\\adddot\n\
             \organizers|organitzadors|org\\adddot\n\
             \byorganizer|organitzat per|org\\adddotspace per\n\
             \byauthor|per|per\n\
             \byeditor|edici\\'o a cura \\smartof|ed\\adddotspace\\smartof\n\
             \bycompiler|compilaci\\'o a cura \\smartof|comp\\adddotspace\\smartof\n\
             \byredactor|redacci\\'o a cura \\smartof|red\\adddotspace\\smartof\n\
             \byreviser|revisi\\'o a cura \\smartof|rev\\adddotspace\\smartof\n\
             \byreviewer|ressenya a cura \\smartof|ress\\adddotspace\\smartof\n\
             \byfounder|fundat per|fund\\adddotspace per\n\
             \bycontinuator|continuat per|cont\\adddotspace per\n\
             \bycollaborator|amb la co\\l.laboraci\\'o \\smartof|amb la co\\l.l\\adddotspace\\smartof\n\
             \bytranslator|traducci\\'o \\lbx@fromlang\\ a cura \\smartof|trad\\adddotspace\\lbx@fromlang\\ \\smartof\n\
             \bycommentator|comentari a cura \\smartof|com\\adddotspace\\smartof\n\
             \byannotator|notes a cura \\smartof|n\\adddotspace\\smartof\n\
             \withcommentator|amb un comentari a cura \\smartof|amb un com\\adddotspace\\smartof\n\
             \withannotator|amb notes a cura \\smartof|amb n\\adddotspace\\smartof\n\
             \withintroduction|amb una introducci\\'o a cura \\smartof|amb una intr\\adddotspace\\smartof\n\
             \withforeword|amb un pr\\`oleg a cura \\smartof|amb un pr\\`ol\\adddotspace\\smartof\n\
             \and|i|i\n\
             \andothers|et al\\adddot|et al\\adddot\n\
             \andmore|i m\\'es|i m\\'es\n\
             \volume|volum|vol\\adddot\n\
             \volumes|volums|vol\\adddot\n\
             \involumes|en|en\n\
             \jourvol|volum|vol\\adddot\n\
             \jourser|s\\`erie|s\\`er\\adddot\n\
             \book|llibre|llib\\adddot\n\
             \part|part|part\n\
             \issue|n\\'umero|n\\'um\\adddot\n\
             \newseries|s\\`erie nova|s\\`erie nova\n\
             \oldseries|s\\`erie antiga|s\\`erie ant\\adddot\n\
             \edition|edici\\'o|ed\\adddot\n\
             \reprint|reimpressi\\'o|reimpr\\adddot\n\
             \reprintof|reimpressi\\'o \\smartof|reimpr\\adddotspace\\smartof\n\
             \reprintas|reimpr\\`es com|reimpr\\adddotspace com\n\
             \reprintfrom|reimpr\\`es \\smartof|reimpr\\adddotspace\\smartof\n\
             \reviewof|ressenya \\smartof|ress\\adddotspace\\smartof\n\
             \translationof|traducci\\'o \\smartof|trad\\adddotspace\\smartof\n\
             \origpubas|publicat origin\\`ariament com|pub\\adddotspace orig\\adddotspace com\n\
             \origpubin|publicat origin\\`ariament el|pub\\adddotspace orig\\adddotspace el\n\
             \astitle|com|com\n\
             \bypublisher|per|per\n\
             \page|p\\`agina|p\\`ag\\adddot\n\
             \pages|p\\`agines|p\\`ag\\adddot\n\
             \column|columna|col\\adddot\n\
             \columns|columnes|cols\\adddot\n\
             \nodate|sine data|s\\adddotspace d\\adddot\n\
             \verse|vers|v\\adddot\n\
             \verses|versos|v\\adddot\n\
             \section|apartat|\\S\n\
             \sections|apartats|\\S\n\
             \paragraph|par\\`agraf|\\P\n\
             \paragraphs|par\\`agrafs|\\P\n\
             \pagetotal|p\\`agina|p\\`ag\\adddot\n\
             \pagetotals|p\\`agines|p\\`ag\\adddot\n\
             \columntotal|columna|col\\adddot\n\
             \columntotals|columnes|col\\adddot\n\
             \versetotal|vers|v\\adddot\n\
             \versetotals|versos|v\\adddot\n\
             \sectiontotal|secci\\'o|\\S\n\
             \sectiontotals|seccions|\\S\n\
             \paragraphtotal|par\\`agrafs|\\P\n\
             \paragraphtotals|par\\`agrafs|\\P\n\
             \in|a|a\n\
             \inseries|a|a\n\
             \ofseries|\\smartof|\\smartof\n\
             \number|n\\'umero|n\\'um\\adddot\n\
             \bathesis|treball final de grau|treb\\adddotspace fin\\adddotspace de gr\\adddot\n\
             \mathesis|treball final de m\\`aster|treb\\adddotspace fin\\adddotspace de m\\`ast\\adddot\n\
             \phdthesis|tesi doctoral|tesi doct\\adddot\n\
             \candthesis|tesi de candidatura|tesi de cand\\adddot\n\
             \resreport|informe de recerca|inf\\adddotspace de rec\\adddot\n\
             \techreport|informe t\\`ecnic|inf\\adddotspace t\\`ec\\adddot\n\
             \software|programari|prog\\adddot\n\
             \datacd|CD de dades|CD de dades\n\
             \audiocd|CD d'\\`audio|CD d'\\`audio\n\
             \version|versi\\'o|vers\\adddot\n\
             \urlfrom|disponible a|disp\\adddotspace a\n\
             \urlseen|consultat|cons\\adddot\n\
             \inpreparation|en preparaci\\'o|en prep\\adddot\n\
             \inpress|a impremta|a impr\\adddot\n\
             \prepublished|pre-publicat|pre-publicat\n\
             \submitted|enviat a publicar|env\\adddotspace a pub\\adddot\n\
             \forthcoming|properament|prop\\adddot\n\
             \citedas|d'ara endavant citat com|d'ara end\\adddotspace cit\\adddotspace com\n\
             \thiscite|concretament|concr\\adddot\n\
             \seenote|vegeu la nota|v\\adddotspace la n\\adddot\n\
             \quotedin|citat a|cit\\adddotspace a\n\
             \opcit|\\`op\\adddotspace cit\\adddot|\\`op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|seq\\adddot|seq\\adddot\n\
             \sequentes|et seq\\adddot|et seq\\adddot\n\
             \passim|p\\`assim|p\\`assim\n\
             \see|vegeu|v\\adddot\n\
             \seealso|vegeu tamb\\'e|v\\adddotspace tamb\\'e\n\
             \backrefpage|vegeu la p\\`agina|v\\adddotspace la p\\`ag\\adddot\n\
             \backrefpages|vegeu les p\\`agines|v\\adddotspace les p\\`ag\\adddot\n\
             \january|gener|gen\\adddot\n\
             \february|febrer|febr\\adddot\n\
             \april|abril|abr\\adddot\n\
             \may|maig|maig\n\
             \june|juny|juny\n\
             \july|juliol|jul\\adddot\n\
             \august|agost|ag\\adddot\n\
             \september|setembre|set\\adddot\n\
             \october|octubre|oct\\adddot\n\
             \november|novembre|nov\\adddot\n\
             \december|desembre|des\\adddot\n\
             \langamerican|angl\\`es americ\\`a|ang\\adddotspace amer\\adddot\n\
             \langbrazilian|portugu\\`es brasiler|port\\adddotspace bras\\adddot\n\
             \langbulgarian|b\\'ulgar|b\\'ulg\\adddot\n\
             \langcatalan|catal\\`a|cat\\adddot\n\
             \langczech|txec|txec\n\
             \langcroatian|croat|croat\n\
             \langdanish|dan\\`es|dan\\adddot\n\
             \langdutch|neerland\\`es|neerl\\adddot\n\
             \langenglish|angl\\`es|ang\\adddot\n\
             \langestonian|estoni\\`a|eston\\adddot\n\
             \langfinnish|fin\\`es|fin\\adddot\n\
             \langfrench|franc\\`es|fr\\adddot\n\
             \langgalician|gallec|gal\\adddot\n\
             \langgerman|alemany|al\\adddot\n\
             \langgreek|grec|grec\n"#)),
       ("croatian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 6744)
             "bibliography|Bibliografija|Bibliografija\n\
             \references|Literatura|Literatura\n\
             \shorthands|Popis kratica|Kratice\n\
             \editor|urednik|ur\\adddot\n\
             \editors|urednici|ur\\adddot\n\
             \redactor|redaktor|redaktor\n\
             \redactors|redaktori|redaktori\n\
             \reviser|korektor|korektor\n\
             \revisers|korektori|korektori\n\
             \collaborator|suradnik|sur\\adddot\n\
             \collaborators|suradnici|sur\\adddot\n\
             \translator|prijevod|prev\\adddot\n\
             \translators|prijevod|prev\\adddot\n\
             \commentator|komentator|komentator\n\
             \commentators|komentatori|komentatori\n\
             \commentary|komentar|komentar\n\
             \introduction|uvod|uvod\n\
             \foreword|predgovor|predgovor\n\
             \afterword|pogovor|pogovor\n\
             \editortr|prijevod i obrada|ur\\adddotspace i prev\\adddot\n\
             \editorstr|prijevod i obrada|ur\\adddotspace i prev\\adddot\n\
             \organizer|organizacija|organizacija\n\
             \organizers|organizacija|organizacija\n\
             \byorganizer|organizacija|organizacija\n\
             \byauthor|autor|autor\n\
             \byeditor|obrada|ur\\adddot\n\
             \byredactor|redaktura|red\\adddot\n\
             \byreviser|korektura|kor\\adddot\n\
             \byreviewer|recenzija|recenzija\n\
             \bycontinuator|nastavio|nastavio\n\
             \bycollaborator|u suradnji s|u sur\\adddotspace s\n\
             \bycommentator|komentari|komentari\n\
             \withcommentator|komentari|komentari\n\
             \withintroduction|uvod|uvod\n\
             \withforeword|predgovor|predgovor\n\
             \withafterword|pogovor|pogovor\n\
             \and|i|i\n\
             \andothers|i drugi|i dr\\adddot\n\
             \andmore|i drugi|i dr\\adddot\n\
             \volume|sv\\adddot|sv\\adddot\n\
             \volumes|sv\\adddot|sv\\adddot\n\
             \involumes|u|u\n\
             \jourvol|sv\\adddot|sv\\adddot\n\
             \jourser|serija|serija\n\
             \book|knjiga|knj\\adddot\n\
             \part|dio|dio\n\
             \issue|izd\\adddot|izd\\adddot\n\
             \newseries|nova serija|nova serija\n\
             \oldseries|stara serija|stara serija\n\
             \edition|izdanje|izdanje\n\
             \reprint|pretisak|pretisak\n\
             \reprintof|pretisak|pretisak\n\
             \reprintas|iznova otisnuto kao|iznova otisnuto kao\n\
             \reprintfrom|iznova otisnut|iznova otisnut\n\
             \translationof|prijevod|prijevod\n\
             \translationas|prev\\adddotspace kao|prev\\adddotspace kao\n\
             \translationfrom|prijevod|prijevod\n\
             \reviewof|osvrt na|osvrt na\n\
             \origpubas|izvorno obj\\adddotspace kao|izv\\adddotspace obj\\adddotspace kao\n\
             \origpubin|izvorno obj\\adddot|izv\\adddotspace obj\\adddot\n\
             \astitle|kao|kao\n\
             \bypublisher||\n\
             \nodate|bez datuma|bez datuma\n\
             \page|stranica|str\\adddot\n\
             \pages|stranice|str\\adddot\n\
             \column|stupac|stupac\n\
             \columns|stupci|stupci\n\
             \line|linija|l\\adddot\n\
             \lines|linije|ll\\adddot\n\
             \verse|stih|stih\n\
             \verses|stihovi|stihovi\n\
             \section|odjeljak|\\S\n\
             \sections|odjeljci|\\S\\S\n\
             \paragraph|stavak|st\\adddot\n\
             \paragraphs|stavci|st\\adddot\n\
             \pagetotal|str\\adddot|str\\adddot\n\
             \pagetotals|str\\adddot|str\\adddot\n\
             \columntotal|stupac|stupac\n\
             \columntotals|stupaca|stupaca\n\
             \linetotal|linija|lin\\adddot\n\
             \linetotals|linija|lin\\adddot\n\
             \versetotal|stih|st\\adddot\n\
             \versetotals|stihova|st\\adddot\n\
             \sectiontotal|odjeljak|odj\\adddot\n\
             \sectiontotals|odjeljaka|odj\\adddot\n\
             \paragraphtotal|stavak|st\\adddot\n\
             \paragraphtotals|stavaka|st\\adddot\n\
             \in||\n\
             \inseries|u seriji|u seriji\n\
             \ofseries|iz serije|iz serije\n\
             \number|broj|br\\adddot\n\
             \chapter|poglavlje|pogl\\adddot\n\
             \bathesis|diplomski rad|dipl\\adddotspace rad\n\
             \mathesis|magistarski rad|mag\\adddotspace rad\n\
             \phdthesis|doktorska disertacija|disertacija\n\
             \software|softver|softver\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|audio CD|audio CD\n\
             \version|verzija|verzija\n\
             \url|adresa|adresa\n\
             \urlfrom|dostupno na|dost\\adddotspace na\n\
             \urlseen|pogledano|pogledano\n\
             \inpreparation|u pripremi|u pripremi\n\
             \submitted|predan|predan\n\
             \inpress|u tisku|u tisku\n\
             \prepublished|pred objavom|pred obj\\adddot\n\
             \citedas|dalje citirano kao|dalje citirano kao\n\
             \thiscite|napose|napose\n\
             \quotedin|citirano u|citirano u\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|usporedi|usp\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \see|vidi|v\\adddot\n\
             \seealso|pogledaj i|pogl\\adddotspace i\n\
             \backrefpage|citirano na stranci|cit\\adddotspace na str\\adddot\n\
             \backrefpages|cited on pages|cit\\adddotspace na str\\adddot\n\
             \april|travanj|travanj\n\
             \may|svibanj|svibanj\n\
             \june|lipanj|lipanj\n\
             \july|srpanj|srpanj\n\
             \august|kolovoz|kolovoz\n\
             \september|rujan|rujan\n\
             \october|listopad|listopad\n\
             \november|studeni|studeni\n\
             \december|prosinac|prosinac\n\
             \dateapril|travnja|travnja\n\
             \datemay|svibnja|svibnja\n\
             \datejune|lipnja|lipnja\n\
             \datejuly|srpnja|srpnja\n\
             \dateaugust|kolovoza|kolovoza\n\
             \dateseptember|rujna|rujna\n\
             \dateoctober|listopada|listopada\n\
             \datenovember|studenoga|studenoga\n\
             \datedecember|prosinca|prosinca\n\
             \langamerican|engleski|engleski\n\
             \langbrazilian|portugalski|portugalski\n\
             \langbulgarian|bugarski|bugarski\n\
             \langcatalan|katalonski|katalonski\n\
             \langcroatian|hrvatski|hrvatski\n\
             \langdanish|danski|danski\n\
             \langdutch|nizozemski|nizozemski\n\
             \langenglish|engleski|engleski\n\
             \langestonian|estonski|estonski\n\
             \langfinnish|finski|finski\n\
             \langfrench|francuski|francuski\n\
             \langgalician|galicijski|galicijski\n\
             \langhungarian|ma\\dj arski|ma\\dj arski\n\
             \langitalian|talijanski|talijanski\n\
             \langjapanese|japanski|japanski\n\
             \langlatin|latinski|latinski\n\
             \langlatvian|letonski|letonski\n\
             \langpolish|poljski|poljski\n\
             \langportuguese|portugalski|portugalski\n\
             \langrussian|ruski|ruski\n\
             \langslovene|slovenski|slovenski\n\
             \langukrainian|ukrajinski|ukrajinski\n\
             \fromamerican|s engleskog|s engleskog\n\
             \frombrazilian|s portugalskog|s portugalskog\n\
             \frombulgarian|s bugarskog|s bugarskog\n\
             \fromcatalan|s katalonskog|s katalonskog\n\
             \fromcroatian|s hrvatskog|s hrvatskog\n\
             \fromdanish|s danskog|s danskog\n\
             \fromdutch|s nizozemskog|s nizozemskog\n\
             \fromenglish|s engleskog|s engleskog\n\
             \fromestonian|s estonskog|s estonskog\n\
             \fromfinnish|s finskog|s finskog\n\
             \fromfrench|s francuskog|s francuskog\n\
             \fromgalician|s galicijskog|s galicijskog\n\
             \fromhungarian|s ma\\dj arskog|s ma\\dj arskog\n\
             \fromitalian|s talijanskog|s talijanskog\n\
             \fromjapanese|s japanskog|s japanskog\n\
             \fromlatin|s latinskog|s latinskog\n\
             \fromlatvian|s letonskog|s letonskog\n\
             \frompolish|s poljskog|s poljskog\n\
             \fromportuguese|s portugalskog|s portugalskog\n\
             \fromrussian|s ruskog|s ruskog\n\
             \fromslovene|sa slovenskog|sa slovenskog\n\
             \fromukrainian|s ukrajinskog|s ukrajinskog\n\
             \countryeu|Europska unija|EU\n\
             \countryep|Europska unija|EP\n\
             \countryfr|Francuska|FR\n\
             \countryuk|Velika Britanija|GB\n\
             \patent|patent|pat\\adddot\n\
             \patenteu|europski patent|europski pat\\adddot\n\
             \patentfr|francuski patent|francuski pat\\adddot\n\
             \patentuk|britanski patent|britanski pat\\adddot\n\
             \patentus|US patent|US pat\\adddot\n\
             \patreq|patentni zahtjev|pat\\adddot\\ zahtjev\n\
             \patreqeu|europski patentni zahtjev|EU pat\\adddotspace zahtjev\n\
             \patrequs|US patentni zahtjev|US pat\\adddot\\ zahtjev\n\
             \file|datoteka|datoteka\n\
             \library|biblioteka|biblioteka\n\
             \commonera|n\\adddotspace e\\adddot|n\\adddotspace e\\adddot\n\
             \beforecommonera|pr\\adddotspace n\\adddotspace e\\adddot|pr\\adddotspace n\\adddotspace e\\adddot\n\
             \annodomini|poslije Krista|p\\adddotspace Kr\\adddot\n\
             \beforechrist|prije Krista|pr\\adddotspace Kr\\adddot\n\
             \circa|oko|cca\\adddot\n\
             \summer|ljeto|ljeto\n\
             \autumn|jesen|jesen\n\
             \winter|zima|zima\n\
             \am|prije podne|AM\n\
             \pm|poslije podne|PM\n"#)),
       ("czech.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 2511)
             "bibliography|Bibliografie|Bibliografie\n\
             \references|Odkazy|Odkazy\n\
             \shorthands|Seznam zkratek|Zkratky\n\
             \editor|editor|ed\\adddot\n\
             \compiler|sestavil|sest\\adddot\n\
             \compilers|sestavili|sest\\adddot\n\
             \redactor|redaktor|red\\adddot\n\
             \reviser|korektor|kor\\adddot\n\
             \founder|zakladatel|zakl\\adddot\n\
             \annotations|anotace|anot\\adddot\n\
             \afterword|doslov|dosl\\adddot\n\
             \byauthor||\n\
             \bycompiler|sestaveno|sest\\adddot\n\
             \withannotator|s\\addnbspace anotacemi od|s\\addnbspace anot\\adddot\\ od\n\
             \withafterword|s\\addnbspace doslovem od|s\\addnbspace dosl\\adddot\\ od\n\
             \and|a|a\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \volume|svazek|sv\\adddot\n\
             \volumes|svazky|sv\\adddot\n\
             \involumes|in|in\n\
             \book|kniha|kniha\n\
             \reprint|dotisk|dotisk\n\
             \reprintof|dotisk|dotisk\n\
             \reprintas|dotisknuto jako|dotisknuto jako\n\
             \reprintfrom|dotisknuto z\\addnbspace|dotisknuto z\\addnbspace\n\
             \reviewof|recenze|recenz\\adddot\n\
             \astitle|jako|jako\n\
             \bypublisher||\n\
             \page|strana|s\\adddot\n\
             \pages|strany|s\\adddot\n\
             \column|sloupec|sl\\adddot\n\
             \columns|sloupce|sl\\adddot\n\
             \section|sekce|\\S\n\
             \sections|sekce|\\S\\S\n\
             \paragraph|odstavec|ods\\adddot\n\
             \paragraphs|odstavce|ods\\adddot\n\
             \pagetotal|strana|s\\adddot\n\
             \pagetotals|strany|s\\adddot\n\
             \columntotal|sloupec|sl\\adddot\n\
             \columntotals|sloupce|sl\\adddot\n\
             \sectiontotal|sekce|\\S\n\
             \sectiontotals|sekce|\\S\\S\n\
             \paragraphtotal|odstavec|ods\\adddot\n\
             \paragraphtotals|odstavce|ods\\adddot\n\
             \in|in|in\n\
             \inseries|in|in\n\
             \ofseries|z|z\n\
             \chapter|kapitola|kap\\adddot\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|audio CD|audio CD\n\
             \version|verze|ver\\adddot\n\
             \url|URL|URL\n\
             \inpress|v\\addnbspace tisku|v\\addnbspace tisku\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \see|viz|viz\n\
             \january|leden|led\\adddot\n\
             \april|duben|dub\\adddot\n\
             \august|srpen|srp\\adddot\n\
             \november|listopad|lis\\adddot\n\
             \december|prosinec|pros\\adddot\n\
             \langlatin|latina|lat\\adddot\n\
             \fromlatin|z\\addnbspace latiny|z\\addnbspace lat\\adddot\n\
             \countryfr|Francie|FR\n\
             \patent|patent|pat\\adddot\n\
             \patentus|US patent|US pat\\adddot\n\
             \file|soubor|soub\\adddot\n\
             \library|knihovna|knih\\adddot\n\
             \abstract|abstrakt|abst\\adddot\n\
             \annotation|anotace|anot\\adddot\n\
             \annodomini|po Kristu|po Kr\\adddot\n\
             \circa|cirka|ca\\adddot\n\
             \spring|jaro|jaro\n\
             \autumn|podzim|podzim\n\
             \winter|zima|zima\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("danish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 3621)
             "bibliography|Bibliografi|Bibliografi\n\
             \references|Litteraturhenvisninger|Litteratur\n\
             \shorthands|Forkortelser|Forkortelser\n\
             \editor|redakt\\o r|red\\adddot\n\
             \editors|redakt\\o rer|red\\adddot\n\
             \compiler|kompilator|kompilator\n\
             \compilers|kompilatorer|kompilatorer\n\
             \redactor|medredakt\\o r|medred\\adddot\n\
             \redactors|medredakt\\o rer|medred\\adddot\n\
             \reviser|revision|rev\\adddot\n\
             \revisers|revision|rev\\adddot\n\
             \founder|stifter|stifter\n\
             \founders|stiftere|stiftere\n\
             \continuator|videref\\o rer|videref\\adddot\n\
             \continuators|videref\\o rere|videref\\adddot\n\
             \collaborator|medarbejder|medarb\\adddot\n\
             \collaborators|medarbejdere|medarb\\adddot\n\
             \translator|overs\\ae tter|overs\\adddot\n\
             \translators|overs\\ae ttere|overs\\adddot\n\
             \commentator|kommentator|komm\\adddot\n\
             \commentators|kommentatorer|komm\\adddot\n\
             \annotator|kommentator|komm\\adddot\n\
             \annotators|kommentatorer|komm\\adddot\n\
             \commentary|kommentar|komm\\adddot\n\
             \annotations|annoteringer|ann\\adddot\n\
             \introduction|indledning|indl\\adddot\n\
             \foreword|forord|forord\n\
             \afterword|efterord|efterord\n\
             \organizer|organisator|org\\adddot\n\
             \organizers|organisatorer|org\\adddot\n\
             \byorganizer|organiseret af|org\\adddotspace av\n\
             \byauthor|af|af\n\
             \byeditor|redigeret af|red\\adddotspace af\n\
             \bycompiler|kompileret af|komp\\adddotspace af\n\
             \byredactor|bearbejdet af|bearb\\adddotspace af\n\
             \byreviser|revideret af|rev\\adddotspace af\n\
             \byreviewer|bed\\o mt af|bed\\adddotspace af\n\
             \byfounder|stiftet af|stiftet af\n\
             \bycontinuator|videref\\o rt af|videref\\adddotspace af\n\
             \bycollaborator|i samarbejde med|i samarb\\adddotspace m\\adddot\n\
             \bycommentator|kommenteret af|komm\\adddot\\ af\n\
             \byannotator|annoteret af|ann\\adddot\\ af\n\
             \withcommentator|med kommentarer af|m\\adddotspace komm\\adddot\\ af\n\
             \withannotator|med annoteringer af|m\\adddotspace ann\\adddot\\ af\n\
             \withintroduction|med indledning af|m\\adddotspace indl\\adddot\\ af\n\
             \withforeword|med forord af|m\\adddotspace forord af\n\
             \withafterword|med efterord af|m\\adddotspace efterord af\n\
             \and|og|og\n\
             \andothers|med flere|m.fl\\adddot\n\
             \andmore|med mere|m.m\\adddot\n\
             \volume|bind|bd\\adddot\n\
             \volumes|bind|bd\\adddot\n\
             \involumes|i|i\n\
             \jourvol|\\aa rgang|\\aa rg\\adddot\n\
             \jourser|r\\ae kke|rk\\adddot\n\
             \book|bog|bog\n\
             \part|del|del\n\
             \issue|nummer|nr\\adddot\n\
             \newseries|ny r\\ae kke|ny rk\\adddot\n\
             \oldseries|gammel r\\ae kke|gl. rk\\adddot\n\
             \edition|udgave|udg\\adddot\n\
             \reprint|genoptryk|genoptr\\adddot\n\
             \reprintof|genoptryk af|genoptr\\adddot\\ af\n\
             \reprintas|genoptryk under titlen|genoptr\\adddot\\ under titlen\n\
             \reprintfrom|genoptryk fra|genoptr\\adddot\\ fra\n\
             \translationof|overs\\ae ttelse af|overs\\adddot\\ af\n\
             \translationas|oversat under titlen|overs\\adddotspace\\ under titlen\n\
             \translationfrom|oversat fra|overs\\adddotspace\\ fra\n\
             \reviewof|bed\\o mmelse af|bed\\adddotspace af\n\
             \origpubas|oprindeligt udgivet som|opr\\adddot\\ udg\\adddot\\ som\n\
             \origpubin|oprindeligt udgivet i|opr\\adddot\\ udg\\adddot\\ i\n\
             \astitle|under titlen|under titlen\n\
             \bypublisher|af|af\n\
             \nodate|uden \\aa rstal|u.\\aa\\adddot\n\
             \page|side|s\\adddot\n\
             \pages|sider|s\\adddot\n\
             \column|spalte|sp\\adddot\n\
             \columns|spalter|sp\\adddot\n\
             \line|linje|l\\adddot\n\
             \lines|linjer|l\\adddot\n\
             \verse|vers|v\\adddot\n\
             \verses|vers|v\\adddot\n\
             \section|paragraf|par\\adddot\n\
             \sections|paragraffer|par\\adddot\n\
             \paragraph|afsnit|afsn\\adddot\n\
             \paragraphs|afsnit|afsn\\adddot\n\
             \pagetotal|side|s\\adddot\n\
             \pagetotals|sider|s\\adddot\n\
             \columntotal|spalte|sp\\adddot\n\
             \columntotals|spalter|sp\\adddot\n\
             \line|linje|l\\adddot\n\
             \lines|linjer|l\\adddot\n\
             \verse|vers|v\\adddot\n\
             \verses|vers|v\\adddot\n\
             \linetotal|linje|l\\adddot\n\
             \linetotals|linje|l\\adddot\n\
             \versetotal|vers|v\\adddot\n\
             \versetotals|vers|v\\adddot\n\
             \sectiontotal|paragraf|par\\adddot\n\
             \sectiontotals|paragraffer|par\\adddot\n\
             \paragraphtotal|afsnit|afsn\\adddot\n\
             \paragraphtotals|afsnit|afsn\\adddot\n\
             \in|i|i\n\
             \inseries|i r\\ae kken|i rk\\adddot\n\
             \ofseries|fra r\\ae kken|fra rk\\adddot\n\
             \number|nummer|nr\\adddot\n"#)),
       ("dutch.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 780)
             "bibliography|Bibliografie|Bibliografie\n\
             \references|Referenties|Referenties\n\
             \shorthands|Lijst van afkortingen|Afkortingen\n\
             \editor|redacteur|red\\adddot\n\
             \editors|redacteurs|red\\adddot\n\
             \compiler|samensteller|samenst\\adddot\n\
             \compilers|samenstellers|samenst\\adddot\n\
             \founder|oprichter|opr\\adddot\n\
             \founders|oprichters|opr\\adddot\n\
             \continuator|opvolger|opv\\adddot\n\
             \continuators|opvolgers|opv\\adddot\n\
             \redactor|redacteur|red\\adddot\n\
             \redactors|redacteuren|red\\adddot\n\
             \reviser|revisor|rev\\adddot\n\
             \revisers|revisors|rev\\adddot\n\
             \collaborator|medewerker|med\\adddot\n\
             \collaborators|medewerkers|med\\adddot\n\
             \translator|vertaler|vert\\adddot\n\
             \translators|vertalers|vert\\adddot\n\
             \commentator|commentator|comm\\adddot\n\
             \commentators|commentatoren|comm\\adddot\n\
             \annotator|annotator|annot\\adddot\n\
             \annotators|annotatoren|annot\\adddot\n"#)),
       ("english.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 8833)
             "bibliography|Bibliography|Bibliography\n\
             \references|References|References\n\
             \shorthands|List of Abbreviations|Abbreviations\n\
             \editor|editor|ed\\adddot\n\
             \editors|editors|eds\\adddot\n\
             \compiler|compiler|comp\\adddot\n\
             \compilers|compilers|comp\\adddot\n\
             \redactor|redactor|red\\adddot\n\
             \redactors|redactors|red\\adddot\n\
             \reviser|reviser|rev\\adddot\n\
             \revisers|revisers|rev\\adddot\n\
             \founder|founder|found\\adddot\n\
             \founders|founders|found\\adddot\n\
             \continuator|continued|cont\\adddot\n\
             \continuators|continued|cont\\adddot\n\
             \collaborator|collaborator|collab\\adddot\n\
             \collaborators|collaborators|collab\\adddot\n\
             \translator|translator|trans\\adddot\n\
             \translators|translators|trans\\adddot\n\
             \commentator|commentator|comm\\adddot\n\
             \commentators|commentators|comm\\adddot\n\
             \annotator|annotator|annot\\adddot\n\
             \annotators|annotators|annot\\adddot\n\
             \commentary|commentary|comm\\adddot\n\
             \annotations|annotations|annot\\adddot\n\
             \introduction|introduction|intro\\adddot\n\
             \foreword|foreword|forew\\adddot\n\
             \afterword|afterword|afterw\\adddot\n\
             \organizer|organizer|org\\adddot\n\
             \organizers|organizers|orgs\\adddot\n\
             \byorganizer|organized by|org\\adddotspace by\n\
             \byauthor|by|by\n\
             \byeditor|edited by|ed\\adddotspace by\n\
             \bycompiler|compiled by|comp\\adddotspace by\n\
             \byredactor|redacted by|red\\adddotspace by\n\
             \byreviser|revised by|rev\\adddotspace by\n\
             \byreviewer|reviewed by|rev\\adddotspace by\n\
             \byfounder|founded by|found\\adddotspace by\n\
             \bycontinuator|continued by|cont\\adddotspace by\n\
             \bycollaborator|in collaboration with|in collab\\adddotspace with\n\
             \bytranslator|translated \\lbx@lfromlang\\ by|trans\\adddot\\ \\lbx@sfromlang\\ by\n\
             \bycommentator|commented by|comm\\adddot\\ by\n\
             \byannotator|annotated by|annot\\adddot\\ by\n\
             \withcommentator|with a commentary by|with a comment\\adddot\\ by\n\
             \withannotator|with annotations by|with annots\\adddot\\ by\n\
             \withintroduction|with an introduction by|with an intro\\adddot\\ by\n\
             \withforeword|with a foreword by|with a forew\\adddot\\ by\n\
             \withafterword|with an afterword by|with an afterw\\adddot\\ by\n\
             \and|and|and\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \volume|volume|vol\\adddot\n\
             \volumes|volumes|vols\\adddot\n\
             \involumes|in|in\n\
             \jourvol|volume|vol\\adddot\n\
             \jourser|series|ser\\adddot\n\
             \book|book|book\n\
             \part|part|part\n\
             \issue|issue|issue\n\
             \newseries|new series|new ser\\adddot\n\
             \oldseries|old series|old ser\\adddot\n\
             \edition|edition|ed\\adddot\n\
             \reprint|reprint|repr\\adddot\n\
             \reprintof|reprint of|repr\\adddotspace of\n\
             \reprintas|reprinted as|rpt\\adddotspace as\n\
             \reprintfrom|reprinted from|repr\\adddotspace from\n\
             \reviewof|review of|rev\\adddotspace of\n\
             \translationof|translation of|trans\\adddotspace of\n\
             \translationas|translated as|trans\\adddotspace as\n\
             \translationfrom|translated from|trans\\adddotspace from\n\
             \origpubas|originally published as|orig\\adddotspace pub\\adddotspace as\n\
             \origpubin|originally published in|orig\\adddotspace pub\\adddotspace in\n\
             \astitle|as|as\n\
             \bypublisher|by|by\n\
             \nodate|no date|n\\adddot d\\adddot\n\
             \page|page|p\\adddot\n\
             \pages|pages|pp\\adddot\n\
             \column|column|col\\adddot\n\
             \columns|columns|cols\\adddot\n\
             \line|line|l\\adddot\n\
             \lines|lines|ll\\adddot\n\
             \verse|verse|v\\adddot\n\
             \verses|verses|vv\\adddot\n\
             \section|section|\\S\n\
             \sections|sections|\\S\\S\n\
             \paragraph|paragraph|par\\adddot\n\
             \paragraphs|paragraphs|par\\adddot\n\
             \pagetotal|page|p\\adddot\n\
             \pagetotals|pages|pp\\adddot\n\
             \columntotal|column|col\\adddot\n\
             \columntotals|columns|cols\\adddot\n\
             \linetotal|line|l\\adddot\n\
             \linetotals|lines|ll\\adddot\n\
             \versetotal|verse|v\\adddot\n\
             \versetotals|verses|vv\\adddot\n\
             \sectiontotal|section|\\S\n\
             \sectiontotals|sections|\\S\\S\n\
             \paragraphtotal|paragraph|par\\adddot\n\
             \paragraphtotals|paragraphs|par\\adddot\n\
             \in|in|in\n\
             \inseries|in|in\n\
             \ofseries|of|of\n\
             \number|number|no\\adddot\n\
             \chapter|chapter|chap\\adddot\n\
             \bathesis|Bachelor's thesis|BA\\addabbrvspace thesis\n\
             \mathesis|Master's thesis|MA\\addabbrvspace thesis\n\
             \phdthesis|PhD\\addabbrvspace thesis|PhD\\addabbrvspace thesis\n\
             \candthesis|Candidate thesis|Cand\\adddotspace thesis\n\
             \resreport|research report|research rep\\adddot\n\
             \techreport|technical report|tech\\adddotspace rep\\adddot\n\
             \software|computer software|comp\\adddotspace software\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|audio CD|audio CD\n\
             \version|version|version\n\
             \url|address|address\n\
             \urlfrom|available from|available from\n\
             \urlseen|visited on|visited on\n\
             \inpreparation|in preparation|in preparation\n\
             \submitted|submitted|submitted\n\
             \forthcoming|forthcoming|forthcoming\n\
             \inpress|in press|in press\n\
             \prepublished|pre-published|pre-published\n\
             \citedas|henceforth cited as|henceforth cited as\n\
             \thiscite|especially|esp\\adddot\n\
             \seenote|see note|see n\\adddot\n\
             \quotedin|quoted in|qtd\\adddotspace in\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \see|see|see\n\
             \seealso|see also|see also\n\
             \backrefpage|cited on page|cit\\adddotspace on p\\adddot\n\
             \backrefpages|cited on pages|cit\\adddotspace on pp\\adddot\n\
             \january|January|Jan\\adddot\n\
             \february|February|Feb\\adddot\n\
             \march|March|Mar\\adddot\n\
             \april|April|Apr\\adddot\n\
             \may|May|May\n\
             \june|June|June\n\
             \july|July|July\n\
             \august|August|Aug\\adddot\n\
             \september|September|Sept\\adddot\n\
             \october|October|Oct\\adddot\n\
             \november|November|Nov\\adddot\n\
             \december|December|Dec\\adddot\n\
             \langamerican|American|American\n\
             \langbrazilian|Brazilian|Brazilian\n\
             \langbulgarian|Bulgarian|Bulgarian\n\
             \langcatalan|Catalan|Catalan\n\
             \langcroatian|Croatian|Croatian\n\
             \langczech|Czech|Czech\n\
             \langdanish|Danish|Danish\n\
             \langdutch|Dutch|Dutch\n\
             \langenglish|English|English\n\
             \langestonian|Estonian|Estonian\n\
             \langfinnish|Finnish|Finnish\n\
             \langfrench|French|French\n\
             \langgalician|Galician|Galician\n\
             \langgerman|German|German\n\
             \langgreek|Greek|Greek\n\
             \langhungarian|Hungarian|Hungarian\n\
             \langitalian|Italian|Italian\n\
             \langjapanese|Japanese|Japanese\n\
             \langlatin|Latin|Latin\n\
             \langlatvian|Latvian|Latvian\n\
             \langlithuanian|Lithuanian|Lithuanian\n\
             \langnorwegian|Norwegian|Norwegian\n\
             \langpolish|Polish|Polish\n\
             \langportuguese|Portuguese|Portuguese\n\
             \langrussian|Russian|Russian\n\
             \langserbian|Serbian|Serbian\n\
             \langslovak|Slovak|Slovak\n\
             \langslovene|Slovene|Slovene\n\
             \langspanish|Spanish|Spanish\n\
             \langswedish|Swedish|Swedish\n\
             \langturkish|Turkish|Turkish\n\
             \langukrainian|Ukrainian|Ukrainian\n\
             \fromamerican|from the American|from the American\n\
             \frombrazilian|from the Brazilian|from the Brazilian\n\
             \frombulgarian|from the Bulgarian|from the Bulgarian\n\
             \fromcatalan|from the Catalan|from the Catalan\n\
             \fromcroatian|from the Croatian|from the Croatian\n\
             \fromczech|from the Czech|from the Czech\n\
             \fromdanish|from the Danish|from the Danish\n\
             \fromdutch|from the Dutch|from the Dutch\n\
             \fromenglish|from the English|from the English\n\
             \fromestonian|from the Estonian|from the Estonian\n\
             \fromfinnish|from the Finnish|from the Finnish\n\
             \fromfrench|from the French|from the French\n\
             \fromgalician|from the Galician|from the Galician\n\
             \fromgerman|from the German|from the German\n\
             \fromgreek|from the Greek|from the Greek\n\
             \fromhungarian|from the Hungarian|from the Hungarian\n\
             \fromitalian|from the Italian|from the Italian\n\
             \fromjapanese|from the Japanese|from the Japanese\n\
             \fromlatin|from the Latin|from the Latin\n\
             \fromlatvian|from the Latvian|from the Latvian\n\
             \fromlithuanian|from the Lithuanian|from the Lithuanian\n\
             \fromnorwegian|from the Norwegian|from the Norwegian\n\
             \frompolish|from the Polish|from the Polish\n\
             \fromportuguese|from the Portuguese|from the Portuguese\n\
             \fromrussian|from the Russian|from the Russian\n\
             \fromserbian|from the Serbian|from the Serbian\n\
             \fromslovak|from the Slovak|from the Slovak\n\
             \fromslovene|from the Slovene|from the Slovene\n\
             \fromspanish|from the Spanish|from the Spanish\n\
             \fromswedish|from the Swedish|from the Swedish\n\
             \fromturkish|from the Turkish|from the Turkish\n\
             \fromukrainian|from the Ukrainian|from the Ukrainian\n\
             \countryde|Germany|DE\n\
             \countryeu|European Union|EU\n\
             \countryep|European Union|EP\n\
             \countryfr|France|FR\n\
             \countryuk|United Kingdom|GB\n\
             \countryus|United States of America|US\n\
             \patent|patent|pat\\adddot\n\
             \patentde|German patent|German pat\\adddot\n\
             \patenteu|European patent|European pat\\adddot\n\
             \patentfr|French patent|French pat\\adddot\n\
             \patentuk|British patent|British pat\\adddot\n\
             \patentus|U.S\\adddotspace patent|U.S\\adddotspace pat\\adddot\n\
             \patreq|patent request|pat\\adddot\\ req\\adddot\n\
             \patreqde|German patent request|German pat\\adddot\\ req\\adddot\n\
             \patreqeu|European patent request|European pat\\adddot\\ req\\adddot\n\
             \patreqfr|French patent request|French pat\\adddot\\ req\\adddot\n\
             \patrequk|British patent request|British pat\\adddot\\ req\\adddot\n\
             \patrequs|U.S\\adddotspace patent request|U.S\\adddotspace pat\\adddot\\ req\\adddot\n\
             \file|file|file\n\
             \library|library|library\n\
             \abstract|abstract|abstract\n\
             \annotation|annotations|annotations\n\
             \commonera|Common Era|CE\n\
             \beforecommonera|Before Common Era|BCE\n\
             \annodomini|Anno Domini|AD\n\
             \beforechrist|Before Christ|BC\n\
             \circa|circa|ca\\adddot\n\
             \spring|Spring|Spr\\adddot\n\
             \summer|Summer|Sum\\adddot\n\
             \autumn|Autumn|Aut\\adddot\n\
             \winter|Winter|Win\\adddot\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("estonian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 6292)
             "bibliography|bibliograafia|bibliograafia\n\
             \references|viited|viited\n\
             \editor|toimetaja|toim\\adddot\n\
             \editors|toimetajad|toim\\adddot\n\
             \compiler|koostaja|koost\\adddot\n\
             \compilers|koostajad|koost\\adddot\n\
             \redactor|toimetaja|toim\\adddot\n\
             \redactors|toimetajad|toim\\adddot\n\
             \reviser|toimetaja|toim\\adddot\n\
             \revisers|toimetajad|toim\\adddot\n\
             \founder|asutaja|asutaja\n\
             \founders|asutajad|asutajad\n\
             \commentator|kommenteerija|kommenteerija\n\
             \commentators|kommenteerijad|kommenteerijad\n\
             \annotator|kommenteerija|kommenteerija\n\
             \annotators|kommenteerijad|kommenteerijad\n\
             \commentary|kommentaarid|kommentaarid\n\
             \annotations|kommentaarid|kommentaarid\n\
             \introduction|sissejuhatus|sissejuhatus\n\
             \organizer|korraldaja|korraldaja\n\
             \organizers|korraldajad|korraldajad\n\
             \byorganizer|korraldanud|korraldanud\n\
             \byauthor|kirjutanud|kirj\\adddot\n\
             \byeditor|toimetanud|toim\\adddot\n\
             \bycompiler|koostanud|koost\\adddot\n\
             \byredactor|toimetanud|toim\\adddot\n\
             \byreviewer|vaadanud|vaadanud\n\
             \byfounder|asutanud|asutanud\n\
             \bycommentator|kommenteerinud|kommenteerinud\n\
             \byannotator|kommenteerinud|kommenteerinud\n\
             \withcommentator|koos kommentaaridega|koos kommentaaridega\n\
             \withannotator|koos kommentaaridega|koos kommentaaridega\n\
             \withintroduction|koos tutvustusega|koos tutvustusega\n\
             \and|ja|ja\n\
             \involumes|osas|osas\n\
             \jourser|seeria|seeria\n\
             \book|raamat|raamat\n\
             \part|osa|osa\n\
             \issue|number|nr\\adddot\n\
             \newseries|uus seerias|uus seer\\adddot\n\
             \oldseries|vana seeria|vana seer\\adddot\n\
             \reviewof|arvustus teosest|arvustus teosest\n\
             \origpubas|algselt avaldatud kui|algselt avaldatud kui\n\
             \origpubin|algselt avaldatud aastal|algselt avaldatud aastal\n\
             \astitle|pealkirjaga|pealkirjaga\n\
             \column|veerg|veerg\n\
             \columns|veerud|veerud\n\
             \line|rida|rida\n\
             \lines|read|read\n\
             \verse|salm|salm\n\
             \verses|salmid|salmid\n\
             \section|jagu|jagu\n\
             \sections|jaod|jaod\n\
             \columntotal|veerge kokku|veerge kokku\n\
             \columntotals|veergusid kokku|veergusid kokku\n\
             \linetotal|ridu kokku|ridu kokku\n\
             \linetotals|ridasid kokku|ridasid kokku\n\
             \versetotal|salmi kokku|salmi kokku\n\
             \versetotals|salmesid kokku|salmesid kokku\n\
             \sectiontotal|jagusid kokku|jagusid kokku\n\
             \sectiontotals|jagusid kokku|jagusid kokku\n\
             \in|teoses|teoses\n\
             \inseries|sarjas|sarjas\n\
             \ofseries|sarjast|sarjast\n\
             \number|number|nr\n\
             \resreport|uuringuraport|uuringuraport\n\
             \techreport|tehniline raport|tehniline raport\n\
             \software|tarkvara|tarkvara\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|audioplaat|audioplaat\n\
             \version|versioon|versioon\n\
             \url|aadress|aadress\n\
             \urlfrom|saadaval|saadaval\n\
             \urlseen|vaadatud|vaadatud\n\
             \inpreparation|koostamisel|koostamisel\n\
             \submitted|esitatud|esitatud\n\
             \forthcoming|tulevases|tulevases\n\
             \citedas|edaspidi viidatud kui|edaspidi viidatud kui\n\
             \thiscite|eriti|eriti\n\
             \quotedin|noteeritud|noteeritud\n\
             \see|vaata|vt\n\
             \seealso|vaata ka|vt ka\n\
             \backrefpage|viidatud lehel|viidatud lehel\n\
             \january|jaanuar|jaanuar\n\
             \february|veebruar|veebruar\n\
             \april|aprill|aprill\n\
             \may|mai|mai\n\
             \june|juuni|juuni\n\
             \july|juuli|juuli\n\
             \august|august|august\n\
             \september|september|september\n\
             \october|oktoober|oktoober\n\
             \november|november|november\n\
             \december|detsember|detsember\n\
             \langamerican|Ameerika inglise keel|Ameerika inglise keel\n\
             \langbrazilian|brasiilia keel|brasiilia keel\n\
             \langbulgarian|bulgaaria keel|bulgaaria keel\n\
             \langcatalan|katalaani keel|katalaani keel\n\
             \langcroatian|horvaadi keel|horvaadi keel\n\
             \langdanish|taani keel|taani keel\n\
             \langdutch|hollandi keel|hollandi keel\n\
             \langenglish|inglise keel|inglise keel\n\
             \langestonian|eesti keel|eesti keel\n\
             \langfinnish|soome keel|soome keel\n\
             \langfrench|prantsuse keel|prantsuse keel\n\
             \langgalician|galeegi keel|galeegi keel\n\
             \langgerman|saksa keel|saksa keel\n\
             \langgreek|kreeka keel|kreeka keel\n\
             \langhungarian|ungari keel|ungari keel\n\
             \langitalian|itaalia keel|itaalia keel\n\
             \langjapanese|jaapani keel|jaapani keel\n\
             \langlatin|ladina keel|ladina keel\n\
             \langnorwegian|norra keel|norra keel\n\
             \langpolish|poola keel|poola keel\n\
             \langportuguese|portugali keel|portugali keel\n\
             \langrussian|vene keel|vene keel\n\
             \langslovak|slovakia keel|slovakia keel\n\
             \langslovene|sloveeni keel|sloveeni keel\n\
             \langspanish|hispaania keel|hispaania keel\n\
             \langswedish|rootsi keel|rootsi keel\n\
             \langukrainian|ukraina keel|ukraina keel\n\
             \fromamerican|Ameerika inglise keelest|Ameerika inglise keelest\n\
             \frombrazilian|brasiilia keelest|brasiilia keelest\n\
             \frombulgarian|bulgaaria keelest|bulgaaria keelest\n\
             \fromcatalan|katalani keelest|kalatalani keelest\n\
             \fromcroatian|horvaadi keelest|horvaadi keelest\n\
             \fromdanish|taani keelest|taani keelest\n\
             \fromdutch|hollandi keelest|hollandi keelest\n\
             \fromenglish|inglise keelest|inglise keelest\n\
             \fromestonian|eesti keelest|eesti keelest\n\
             \fromfinnish|soome keelest|soome keelest\n\
             \fromfrench|prantsuse keelest|prantsuse keelest\n\
             \fromgalician|galeegi keelest|galeegi keelest\n\
             \fromgerman|saksa keelest|saksa keelest\n\
             \fromgreek|kreeka keelest|kreeka keelest\n\
             \fromhungarian|ungari keelest|ungari keelest\n\
             \fromitalian|itaalia keelest|itaalia keelest\n\
             \fromjapanese|jaapani keelest|jaapani keelest\n\
             \fromlatin|ladina keelest|ladina keelest\n\
             \fromnorwegian|norra keelest|norra keelest\n\
             \fromportuguese|portugali keelest|portugali keelest\n\
             \fromrussian|vene keelest|vene keelest\n\
             \fromslovak|slovakia keelest|slovakia keelest\n\
             \fromslovene|sloveenia keelest|sloveenia keelest\n\
             \fromspanish|hispaania keelest|hispaania keelest\n\
             \fromswedish|rootsi keelest|rootsi keelest\n\
             \fromukrainian|ukraina keelest|ukraina keelest\n\
             \countryde|Saksamaa|DE\n\
             \countryeu|Euroopa Liit|EL\n\
             \countryep|Euroopa Parlament|EP\n\
             \countryfr|Prantsusmaa|FR\n\
             \countryuk|Suurbritannia|GB\n\
             \countryus|Ameerika|US\n\
             \patent|patent|pat\\adddot\n\
             \patentde|Saksa patent|Saksa pat\\adddot\n\
             \patenteu|Euroopa patent|Euroopa pat\\adddot\n\
             \patentfr|Prantsuse patent|Prantsuse pat\\adddot\n\
             \patentuk|Briti patent|Briti pat\\adddot\n\
             \patentus|U.S\\adddotspace patent|U.S\\adddotspace pat\\adddot\n\
             \patreq|patendi taotlus|pat\\adddotspace taotl\\adddot\n\
             \patreqde|Saksa patendi taotlus|Saksa pat\\adddotspace taotl\\adddot\n\
             \patreqeu|Euroopa patendi taotlus|Euroopa pat\\adddotspace taotl\\adddot\n\
             \patreqfr|Prantsuse patendi taotlus|Prantsuse pat\\adddotspace taotl\\adddot\n\
             \patrequk|Briti patendi taotlus|Briti pat\\adddotspace taotl\\adddot\n\
             \patrequs|U.S\\adddotspace patendi taotlus.|U.S\\adddotspace pat\\adddotspace taotl\\adddot\n\
             \file|fail|fail\n\
             \library|raamatukogu|raamatukogu\n\
             \abstract|teesid|teesid\n\
             \annotation|kommentaarid|kommentaarid\n\
             \commonera|meie ajaarvamise j\\\"argi|m\\adddot a\\adddot j\n\
             \beforecommonera|enne meie ajaarvamist|e\\adddot m\\adddot a\n\
             \annodomini|p\\\"arast Kristust|pKr\n\
             \beforechrist|enne Kristust|eKr\n\
             \spring|kevadel|kevadel\n\
             \summer|suvel|suvel\n\
             \winter|talvel|talvel\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("finnish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 9348)
             "bibliography|Kirjallisuusluettelo|Kirjallisuus\n\
             \references|Viitteet|Viitteet\n\
             \shorthands|Lyhenteet|Lyhenteet\n\
             \editor|toimittanut|toim\\adddot\n\
             \editors|toimittaneet|toim\\adddot\n\
             \compiler|koontanut|koontanut\n\
             \compilers|koontaneet|koontaneet\n\
             \redactor|toimittanut|toim\\adddot\n\
             \redactors|toimittaneet|toim\\adddot\n\
             \reviser|toimittanut|toim\\adddot\n\
             \revisers|toimittaneet|toim\\adddot\n\
             \founder|perustaja|perustaja\n\
             \founders|perustajat|perustajat\n\
             \continuator|jatkaja|jatkaja\n\
             \continuators|jatkajat|jatkajat\n\
             \collaborator|avustaja|avustaja\n\
             \collaborators|avustajat|avustajat\n\
             \translator|k\\\"a\\\"ant\\\"anyt|k\\\"a\\\"ant\\adddot\n\
             \translators|k\\\"a\\\"ant\\\"anyt|k\\\"a\\\"ant\\adddot\n\
             \commentator|kommentaarin kirjoittanut|kommentaarin kirjoittanut\n\
             \commentators|kommentaarin kirjoittaneet|kommentaarin kirjoittaneet\n\
             \annotator|selityksin varustanut|selityksin varustanut\n\
             \annotators|selityksin varustaneet|selityksin varustaneet\n\
             \commentary|kommentaari|kommentaari\n\
             \annotations|selitykset|selitykset\n\
             \introduction|johdanto|johdanto\n\
             \foreword|esipuhe|esipuhe\n\
             \afterword|j\\\"alkisanat|j\\\"alkisanat\n\
             \organizer|toimittanut|toim\\adddot\n\
             \organizers|toimittaneet|toim\\adddot\n\
             \byorganizer|toimittanut|toim\\adddot\n\
             \byauthor|kirjoittanut|kirj\\adddot\n\
             \byeditor|toimittanut|toim\\adddot\n\
             \bycompiler|koontanut|koontanut\n\
             \byredactor|toimittanut|toim\\adddot\n\
             \byreviser|toimittanut|toim\\adddot\n\
             \byreviewer|toimittanut|toim\\adddot\n\
             \byfounder|perustanut|perustanut\n\
             \bycontinuator|jatkanut|jatkanut\n\
             \bycollaborator|yhteisty\\\"oss\\\"a|yhteisty\\\"oss\\\"a\n\
             \bytranslator|\\lbx@lfromlang k\\\"a\\\"ant\\\"anyt|\\lbx@sfromlang k\\\"a\\\"ant\\adddot\n\
             \bycommentator|kommentaarin kirjoittanut|kommentaarin kirjoittanut\n\
             \byannotator|selityksin varustanut|selityksin varustanut\n\
             \withcommentator|kommentaarin kirjoittanut|kommentaarin kirjoittanut\n\
             \withannotator|selityksin varustanut|selityksin varustanut\n\
             \withintroduction|johdannon kirjottanut|johdannon kirjottanut\n\
             \withforeword|esipuheen kirjoittanut|esipuheen kirjoittanut\n\
             \withafterword|j\\\"alkisanat kirjoittanut|j\\\"alkisanat kirjoittanut\n\
             \and|ja|ja\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|jne\\adddot|jne\\adddot\n\
             \volume|volyymi|vol\\adddot\n\
             \volumes|volyymit|vol\\adddot\n\
             \involumes||\n\
             \jourvol|volyymi|vol\\adddot\n\
             \jourser|sarja|sarja\n\
             \book|kirja|kirja\n\
             \part|osa|osa\n\
             \issue|numero|numero\n\
             \newseries|uusi sarja|uusi sarja\n\
             \oldseries|vanha sarja|vanha sarja\n\
             \edition|painos|painos\n\
             \reprint|j\\\"alkipainos|j\\\"alkipainos\n\
             \reprintof|julkaistu aiemmin nimell\\\"a|julkaistu aiemmin nimell\\\"a\n\
             \reprintas|julkaistu uudelleen nimell\\\"a|julkaistu uudelleen nimell\\\"a\n\
             \reprintfrom|julkaistu aiemmin nimell\\\"a|julkaistu aiemmin nimell\\\"a\n\
             \translationof|k\\\"a\\\"ann\\\"os teoksesta|k\\\"a\\\"ann\\\"os teoksesta\n\
             \translationas|k\\\"a\\\"annetty nimell\\\"a|k\\\"a\\\"annetty nimell\\\"a\n\
             \translationfrom|k\\\"a\\\"annetty kielest\\\"a|k\\\"a\\\"annetty kielest\\\"a\n\
             \reviewof|arvostelu teoksesta|arvostelu teoksesta\n\
             \origpubas|julkaistu ensi kerran nimell\\\"a|julkaistu ensi kerran nimell\\\"a\n\
             \origpubin|julkaistu ensi kerran vuonna|julkaistu ensi kerran vuonna\n\
             \astitle|nimell\\\"a|nimell\\\"a\n\
             \bypublisher|julkaissut|julkaissut\n\
             \page|sivu|s\\adddot\n\
             \pages|sivut|s\\adddot\n\
             \column|palsta|palsta\n\
             \columns|palstat|palstat\n\
             \line|rivi|rivi\n\
             \lines|rivit|rivit\n\
             \nodate|ei julkaisup\\\"aiv\\\"a\\\"a|ei julkaisup\\\"aiv\\\"a\\\"a\n\
             \verse|s\\\"ae|s\\\"ae\n\
             \verses|s\\\"akeet|s\\\"akeet\n\
             \section|kohta|kohta\n\
             \sections|kohdat|kohdat\n\
             \paragraph|kappale|kappale\n\
             \paragraphs|kappaleet|kappaleet\n\
             \pagetotal|sivu|s\\adddot\n\
             \pagetotals|sivut|s\\adddot\n\
             \columntotal|palsta|palsta\n\
             \columntotals|palstat|palstat\n\
             \linetotal|rivi|rivi\n\
             \linetotals|rivit|rivit\n\
             \versetotal|s\\\"ae|s\\\"ae\n\
             \versetotals|s\\\"akeet|s\\\"akeet\n\
             \sectiontotal|kohta|kohta\n\
             \sectiontotals|kohdat|kohdat\n\
             \paragraphtotal|kappale|kappale\n\
             \paragraphtotals|kappaleet|kappaleet\n\
             \in|teoksessa|teoksessa\n\
             \inseries|sarjassa|sarjassa\n\
             \ofseries|sarjassa|sarjassa\n\
             \number|numero|nro\n\
             \chapter|luku|luku\n\
             \bathesis|tutkielma|tutkielma\n\
             \mathesis|tutkielma|tutkielma\n\
             \phdthesis|tohtorinv\\\"ait\\\"oskirja|tohtorinv\\\"ait\\\"oskirja\n\
             \candthesis|kanditaatintutkielma|kanditaatintutkielma\n\
             \resreport|tutkimusraportti|tutkimusraportti\n\
             \techreport|tekninen raportti|tekninen raportti\n\
             \software|ohjelmisto|ohjelmisto\n\
             \datacd|data-CD|data-CD\n\
             \audiocd|\\\"a\\\"ani-CD|\\\"a\\\"ani-CD\n\
             \version|versio|versio\n\
             \url|url|url\n\
             \urlfrom|saatavilla osoitteesta|saatavilla osoitteesta\n\
             \urlseen|viitattu|viitattu\n\
             \inpreparation|valmisteilla|valmisteilla\n\
             \submitted|l\\\"ahetetty|l\\\"ahetetty\n\
             \forthcoming|hyv\\\"aksytty julkaistavaksi|hyv\\\"aksytty julkaistavaksi\n\
             \inpress|painossa|painossa\n\
             \prepublished|esijulkaistu|esijulkaistu\n\
             \citedas|jatkossa|jatkossa\n\
             \thiscite|sama|sama\n\
             \seenote|katso viite|katso viite\n\
             \quotedin|lainattu teoksessa|lainattu teoksessa\n\
             \idem|idem|id\\adddot\n\
             \idemsf|idem|id\\adddot\n\
             \idemsm|idem|id\\adddot\n\
             \idemsn|idem|id\\adddot\n\
             \idempf|idem|id\\adddot\n\
             \idempm|idem|id\\adddot\n\
             \idempn|idem|id\\adddot\n\
             \idempp|idem|id\\adddot\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \see|katso|ks\\adddot\n\
             \seealso|katso my\\\"os|katso my\\\"os\n\
             \backrefpage|katso sivu|ks\\adddotspace s\\adddot\n\
             \backrefpages|katso sivut|ks\\adddotspace s\\adddot\n\
             \january|tammikuuta|tammikuuta\n\
             \february|helmikuuta|helmikuuta\n\
             \march|maaliskuuta|maaliskuuta\n\
             \april|huhtikuuta|huhtikuuta\n\
             \may|toukokuuta|toukokuuta\n\
             \june|kes\\\"akuuta|kes\\\"akuuta\n\
             \july|hein\\\"akuuta|hein\\\"akuuta\n\
             \august|elokuuta|elokuuta\n\
             \september|syyskuuta|syyskuuta\n\
             \october|lokakuuta|lokakuuta\n\
             \november|marraskuuta|marraskuuta\n\
             \december|joulukuuta|joulukuuta\n\
             \basicjanuary|tammikuu|tammikuu\n\
             \basicfebruary|helmikuu|helmikuu\n\
             \basicmarch|maaliskuu|maaliskuu\n\
             \basicapril|huhtikuu|huhtikuu\n\
             \basicmay|toukokuu|toukokuu\n\
             \basicjune|kes\\\"akuu|kes\\\"akuu\n\
             \basicjuly|hein\\\"akuu|hein\\\"akuu\n\
             \basicaugust|elokuu|elokuu\n\
             \basicseptember|syyskuu|syyskuu\n\
             \basicoctober|lokakuu|lokakuu\n\
             \basicnovember|marraskuu|marraskuu\n\
             \basicdecember|joulukuu|joulukuu\n\
             \langamerican|amerikanenglanti|amerikanenglanti\n\
             \langbrazilian|brasilianportugali|brasilianportugali\n\
             \langbulgarian|bulgaria|bulgaria\n\
             \langcatalan|katalonia|katalonia\n\
             \langcroatian|kroatia|kroatia\n\
             \langdanish|tanska|tanska\n\
             \langdutch|hollanti|hollanti\n\
             \langenglish|englanti|englanti\n\
             \langestonian|viro|viro\n\
             \langfinnish|suomi|suomi\n\
             \langfrench|ranska|ranska\n\
             \langgalician|galicia|galicia\n\
             \langgerman|saksa|saksa\n\
             \langgreek|kreikka|kreikka\n\
             \langhungarian|unkari|unkari\n\
             \langitalian|italia|italia\n\
             \langlatin|latina|latina\n\
             \langlatvian|latvia|latvia\n\
             \langnorwegian|norja|norja\n\
             \langpolish|puola|puola\n\
             \langportuguese|portugali|portugali\n\
             \langrussian|ven\\\"aj\\\"a|ven\\\"aj\\\"a\n\
             \langslovak|slovakia|slovakia\n\
             \langslovene|slovenia|slovenia\n\
             \langspanish|espanja|espanja\n\
             \langswedish|ruotsi|ruotsi\n\
             \fromamerican|englannin kielest\\\"a|englannin kielest\\\"a\n\
             \frombrazilian|portugalin kielest\\\"a|portugalin kielest\\\"a\n\
             \frombulgarian|bulgarian kielest\\\"a|bulgarian kielest\\\"a\n\
             \fromcatalan|katalonian kielest\\\"a|katalonian kielest\\\"a\n\
             \fromcroatian|kroatian kielest\\\"a|kroatian kielest\\\"a\n\
             \fromdanish|tanskan kielest\\\"a|tanskan kielest\\\"a\n\
             \fromdutch|hollannin kielest\\\"a|hollannin kielest\\\"a\n\
             \fromenglish|englannin kielest\\\"a|englannin kielest\\\"a\n\
             \fromestonian|viron kielest\\\"a|viron kielest\\\"a\n\
             \fromfinnish|suomen kielest\\\"a|suomen kielest\\\"a\n\
             \fromfrench|ranskan kielest\\\"a|ranskan kielest\\\"a\n\
             \fromgalician|galician kielest\\\"a|galician kielest\\\"a\n\
             \fromgerman|saksan kielest\\\"a|saksan kielest\\\"a\n\
             \fromgreek|kreikan kielest\\\"a|kreikan kielest\\\"a\n\
             \fromhungarian|unkarin kielest\\\"a|unkarin kielest\\\"a\n\
             \fromitalian|italian kielest\\\"a|italian kielest\\\"a\n\
             \fromlatin|latinan kielest\\\"a|latinan kielest\\\"a\n\
             \fromlatvian|latvian kielest\\\"a|latvian kielest\\\"a\n\
             \fromnorwegian|norjan kielest\\\"a|norjan kielest\\\"a\n\
             \frompolish|puolan kielest\\\"a|puolan kielest\\\"a\n\
             \fromportuguese|portugalin kielest\\\"a|portugalin kielest\\\"a\n\
             \fromrussian|ven\\\"aj\\\"an kielest\\\"a|ven\\\"aj\\\"an kielest\\\"a\n\
             \fromslovak|slovakian kielest\\\"a|slovakian kielest\\\"a\n\
             \fromslovene|slovenian kielest\\\"a|slovenian kielest\\\"a\n\
             \fromspanish|espanjan kielest\\\"a|espanjan kielest\\\"a\n\
             \fromswedish|ruotsin kielest\\\"a|ruotsin kielest\\\"a\n\
             \countryde|Saksa|DE\n\
             \countryeu|Euroopan Unioni|EU\n\
             \countryep|Euroopan Unioni|EP\n\
             \countryfr|Ranska|FR\n\
             \countryuk|Iso-Britannia|GB\n\
             \countryus|Yhdysvallat|US\n\
             \patent|patentti|pat\\adddot\n\
             \patentde|saksalainen patentti|saksalainen pat\\adddot\n\
             \patenteu|Euroopan Unionin patentti|Euroopan Unionin pat\\adddot\n\
             \patentfr|ranskalainen patentti|ranskalainen pat\\adddot\n\
             \patentuk|isobritannialainen patentti|isobritannialainen pat\\adddot\n\
             \patentus|yhdysvaltalainen patentti|yhdysvaltalainen pat\\adddot\n\
             \patreq|patenttihakemus|pat\\adddot\\ hak\\adddot\n\
             \patreqde|saksalainen patenttihakemus|saksalainen pat\\adddot\\ hak\\adddot\n\
             \patreqeu|Euroopan Unionin patenttihakemus|Euroopan Unionin pat\\adddot\\ hak\\adddot\n\
             \patreqfr|ranskalainen patenttihakemus|ranskalainen pat\\adddot\\ hak\\adddot\n\
             \patrequk|isobritannialainen patenttihakemus|isobritannialainen pat\\adddot\\ hak\\adddot\n\
             \patrequs|yhdysvaltalainen patenttihakemus|yhdysvaltalainen pat\\adddot\\ hak\\adddot\n\
             \file|tiedosto|tiedosto\n\
             \library|kirjasto|kirjasto\n\
             \abstract|tiivistelm\\\"a|tiivistelm\\\"a\n\
             \annotation|selitykset|selitykset\n\
             \commonera|j\\\"alkeen ajanlaskun alun|jaa\\adddot\n\
             \beforecommonera|ennen ajanlaskun alkua|eaa\\adddot\n\
             \annodomini|j\\\"alkeen Kristuksen syntym\\\"an|jKr\\adddot\n\
             \beforechrist|ennen Kristuksen syntym\\\"a\\\"a|eKr\\adddot\n\
             \circa|noin|n\\adddot\n\
             \spring|kev\\\"at|kev\\\"at\n\
             \summer|kes\\\"a|kes\\\"a\n\
             \autumn|syksy|syksy\n\
             \winter|talvi|talvi\n\
             \am|ap\\adddot|ip\\adddot\n\
             \pm|ip\\adddot|ip\\adddot\n"#)),
       ("french.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 9046)
             "bibliography|Bibliographie|Bibliographie\n\
             \references|R\\'ef\\'erences|R\\'ef\\'erences\n\
             \shorthands|Liste des abr\\'eviations|Abr\\'eviations\n\
             \editor|\\'editeur|\\'ed\\adddot\n\
             \editors|\\'editeurs|\\'ed\\adddot\n\
             \compiler|compilateur|comp\\adddot\n\
             \compilers|compilateurs|comp\\adddot\n\
             \redactor|r\\'edacteur|r\\'ed\\adddot\n\
             \redactors|r\\'edacteurs|r\\'ed\\adddot\n\
             \reviser|r\\'eviseur|r\\'ev\\adddot\n\
             \revisers|r\\'eviseurs|r\\'ev\\adddot\n\
             \founder|fondateur|fond\\adddot\n\
             \founders|fondateurs|fond\\adddot\n\
             \continuator|continuateur|cont\\adddot\n\
             \continuators|continuateurs|cont\\adddot\n\
             \collaborator|collaborateur|coll\\adddot\n\
             \collaborators|collaborateurs|coll\\adddot\n\
             \translator|traducteur|trad\\adddot\n\
             \translators|traducteurs|trad\\adddot\n\
             \commentator|commentateur|comm\\adddot\n\
             \commentators|commentateurs|comm\\adddot\n\
             \annotator|annotateur|annot\\adddot\n\
             \annotators|annotateurs|annot\\adddot\n\
             \commentary|commentaires|comment\\adddot\n\
             \annotations|annotations|annot\\adddot\n\
             \introduction|introduction|introd\\adddot\n\
             \foreword|pr\\'eface|pr\\'ef\\adddot\n\
             \afterword|postface|postf\\adddot\n\
             \editortr|\\'editeur et traducteur|\\'ed\\adddotspace et trad\\adddot\n\
             \editorstr|\\'editeurs et traducteurs|\\'ed\\adddotspace et trad\\adddot\n\
             \organizer|organisateur|org\\adddot\n\
             \organizers|organisateurs|org\\adddot\n\
             \byorganizer|organis\\'e par|org\\adddotspace par\n\
             \byauthor|par|par\n\
             \byeditor|sous la direction \\smartof|sous la dir\\adddotspace\\smartof\n\
             \bycompiler|compil\\'e par|comp\\adddotspace par\n\
             \byredactor|r\\'edig\\'e par|r\\'ed\\adddotspace par\n\
             \byreviser|r\\'evis\\'e par|r\\'ev\\adddotspace par\n\
             \byreviewer|examin\\'e par|ex\\adddotspace par\n\
             \byfounder|d\\'ecouvert par|d\\'ecouv\\adddotspace par\n\
             \bycontinuator|continu\\'e par|cont\\adddotspace par\n\
             \bytranslator|traduit \\lbx@lfromlang\\ par|trad\\adddotspace \\lbx@sfromlang\\ par\n\
             \bycommentator|comment\\'e par|comm\\adddotspace par\n\
             \byannotator|annot\\'e par|annot\\adddotspace par\n\
             \withcommentator|avec des commentaires \\smartof|avec des comment\\adddotspace\\smartof\n\
             \withannotator|avec des annotations \\smartof|avec des annot\\adddotspace\\smartof\n\
             \withintroduction|avec une introduction \\smartof|avec une introd\\adddotspace\\smartof\n\
             \withforeword|avec une pr\\'eface \\smartof|avec une pr\\'ef\\adddotspace\\smartof\n\
             \withafterword|avec une postface \\smartof|avec une postf\\adddotspace\\smartof\n\
             \and|et|et\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \volume|tome|t\\adddot\n\
             \volumes|tomes|t\\adddot\n\
             \involumes|en|en\n\
             \jourvol|tome|t\\adddot\n\
             \jourser|s\\'erie|s\\'er\\adddot\n\
             \book|livre|livre\n\
             \part|partie|partie\n\
             \issue|\\'edition|\\'ed\\adddot\n\
             \newseries|nouvelle s\\'erie|nouv\\adddotspace s\\'er\\adddot\n\
             \oldseries|ancienne s\\'erie|anc\\adddotspace s\\'er\\adddot\n\
             \edition|\\'edition|\\'ed\\adddot\n\
             \reprint|r\\'eimpression|r\\'eimpr\\adddot\n\
             \reprintof|r\\'eimpression de|r\\'eimpr\\adddotspace de\n\
             \reprintas|r\\'eimpression sous le titre|r\\'eimpr\\adddotspace sous le titre\n\
             \reprintfrom|r\\'eimpression \\`a partir \\smartof|r\\'eimpr\\adddotspace \\`a part\\adddotspace\\smartof\n\
             \translationof|traduction de|trad\\adddotspace de\n\
             \translationas|traduit sous le titre|trad\\adddotspace sous le titre\n\
             \translationfrom|traduit de|trad\\adddotspace de\n\
             \reviewof|critique de|crit.\\adddotspace de\n\
             \origpubas|publi\\'e \\`a l'origine sous le titre|pub\\adddotspace \\`a l'orig\\adddotspace sous le titre\n\
             \origpubin|publi\\'e \\`a l'origine en|pub\\adddotspace \\`a l'orig\\adddotspace en\n\
             \astitle|sous le titre|sous le titre\n\
             \bypublisher|par|par\n\
             \page|page|p\\adddot\n\
             \pages|pages|p\\adddot\n\
             \column|colonne|col\\adddot\n\
             \columns|colonnes|col\\adddot\n\
             \line|ligne|l\\adddot\n\
             \lines|lignes|l\\adddot\n\
             \verse|vers|v\\adddot\n\
             \verses|vers|v\\adddot\n\
             \section|section|sect\\adddot\n\
             \sections|sections|sect\\adddot\n\
             \paragraph|paragraphe|\\S\n\
             \paragraphs|paragraphes|\\S\n\
             \pagetotal|page|p\\adddot\n\
             \pagetotals|pages|p\\adddot\n\
             \columntotal|colonne|col\\adddot\n\
             \columntotals|colonnes|col\\adddot\n\
             \linetotal|ligne|l\\adddot\n\
             \linetotals|lignes|l\\adddot\n\
             \versetotal|vers|v\\adddot\n\
             \versetotals|vers|v\\adddot\n\
             \sectiontotal|section|sect\\adddot\n\
             \sectiontotals|sections|sect\\adddot\n\
             \paragraphtotal|paragraphe|\\S\n\
             \paragraphtotals|paragraphes|\\S\n\
             \in|in|in\n\
             \inseries|in|in\n\
             \ofseries|de|de\n\
             \number|num\\'ero|n\\textsuperscript{o\n\
             \chapter|chapitre|chap\\adddot\n\
             \bathesis|m\\'emoire de bachelor|m\\'em\\adddotspace de bach\\adddot\n\
             \mathesis|m\\'emoire de master|m\\'em\\adddotspace de mast\\adddot\n\
             \phdthesis|th\\`ese de doctorat|th\\`ese de doct\\adddot\n\
             \candthesis|th\\`ese de candidature|th\\`ese de cand\\adddot\n\
             \resreport|rapport scientifique|rapp\\adddotspace scient\\adddot\n\
             \techreport|rapport technique|rapp\\adddotspace tech\\adddot\n\
             \software|logiciel|logiciel\n\
             \datacd|c\\'ed\\'erom|c\\'ed\\'erom\n\
             \audiocd|disque compact audio|CD\n\
             \version|version|version\n\
             \url|adresse|adresse\n\
             \urlfrom|disponible \\`a l'adresse|disp\\adddotspace \\`a l'adr\\adddot\n\
             \urlseen|visit\\'e le|visit\\'e le\n\
             \inpreparation|en pr\\'eparation|en pr\\'ep\\adddot\n\
             \submitted|soumis|soumis\n\
             \inpress|sous presse|sous presse\n\
             \prepublished|pr\\'epubli\\'e|pr\\'epubl\\adddot\n\
             \citedas|d\\'esign\\'e ci-apr\\`es par|ci-apr\\`es\n\
             \thiscite|sp\\'ecialement|sp\\'ec\\adddot\n\
             \seenote|voir note|cf\\adddotspace note\n\
             \quotedin|cit\\'e dans|cit\\'e dans\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \see|cf\\adddot|cf\\adddot\n\
             \seealso|cf\\adddotspace aussi|cf\\adddotspace aussi\n\
             \backrefpage|cf\\adddotspace page|cf\\adddotspace p\\adddot\n\
             \backrefpages|cf\\adddotspace pages|cf\\adddotspace p\\adddot\n\
             \january|janvier|jan\\adddot\n\
             \february|f\\'evrier|f\\'ev\\adddot\n\
             \march|mars|mars\n\
             \april|avril|avr\\adddot\n\
             \may|mai|mai\n\
             \june|juin|juin\n\
             \july|juillet|juill\\adddot\n\
             \august|ao\\^ut|ao\\^ut\n\
             \september|septembre|sept\\adddot\n\
             \october|octobre|oct\\adddot\n\
             \november|novembre|nov\\adddot\n\
             \december|d\\'ecembre|d\\'ec\\adddot\n\
             \langamerican|am\\'ericain|am\\'ericain\n\
             \langbrazilian|br\\'esilien|br\\'esilien\n\
             \langbulgarian|bulgare|bulgare\n\
             \langcatalan|catalan|catalan\n\
             \langcroatian|croate|croate\n\
             \langczech|tch\\`eque|tch\\`eque\n\
             \langdanish|danois|danois\n\
             \langdutch|n\\'eerlandais|n\\'eerlandais\n\
             \langenglish|anglais|anglais\n\
             \langestonian|estonien|estonien\n\
             \langfinnish|finnois|finnois\n\
             \langgalician|galicien|galicien\n\
             \langgerman|allemand|allemand\n\
             \langgreek|grec|grec\n\
             \langhungarian|hongrois|hongrois\n\
             \langitalian|italien|italien\n\
             \langjapanese|japonais|japonais\n\
             \langlatin|latin|latin\n\
             \langlatvian|letton|letton\n\
             \langnorwegian|norv\\'egien|norv\\'egien\n\
             \langpolish|polonais|polonais\n\
             \langportuguese|portugais|portugais\n\
             \langrussian|russe|russe\n\
             \langserbian|serbe|serbe\n\
             \langslovak|slovaque|slovaque\n\
             \langslovene|slov\\`ene|slov\\`ene\n\
             \langspanish|espagnol|espagnol\n\
             \langswedish|su\\'edois|su\\'edois\n\
             \langukrainian|ukrainien|ukrainien\n\
             \fromamerican|de l'am\\'ericain|de l'am\\'ericain\n\
             \frombrazilian|du br\\'esilien|du br\\'esilien\n\
             \frombulgarian|du bulgare|du bulgare\n\
             \fromcatalan|du catalan|du catalan\n\
             \fromcroatian|du croate|du croate\n\
             \fromczech|du tch\\`eque|du tch\\`eque\n\
             \fromdanish|du danois|du danois\n\
             \fromdutch|du n\\'eerlandais|du n\\'eerlandais\n\
             \fromenglish|de l'anglais|de l'anglais\n\
             \fromestonian|de l'estonien|de l'estonien\n\
             \fromfinnish|du finnois|du finnois\n\
             \fromgalician|du galicien|du galicien\n\
             \fromgerman|de l'allemand|de l'allemand\n\
             \fromgreek|du grec|du grec\n\
             \fromhungarian|du hongrois|du hongrois\n\
             \fromitalian|de l'italien|de l'italien\n\
             \fromjapanese|du japonais|du japonais\n\
             \fromlatin|du latin|du latin\n\
             \fromlatvian|du letton|du letton\n\
             \fromnorwegian|du norv\\'egien|du norv\\'egien\n\
             \frompolish|du polonais|du polonais\n\
             \fromportuguese|du portugais|du portugais\n\
             \fromrussian|du russe|du russe\n\
             \fromserbian|du serbe|du serbe\n\
             \fromslovak|du slovaque|du slovaque\n\
             \fromslovene|du slov\\`ene|du slov\\`ene\n\
             \fromspanish|de l'espagnol|de l'espagnol\n\
             \fromswedish|du su\\'edois|du su\\'edois\n\
             \fromukrainian|de l'ukrainien|de l'ukrainien\n\
             \countryde|Allemagne|DE\n\
             \countryeu|Union europ\\'eenne|EU\n\
             \countryep|Union europ\\'eenne|EP\n\
             \countryfr|France|FR\n\
             \countryuk|Royaume-Uni|GB\n\
             \countryus|\\'Etats-Unis|US\n\
             \patent|brevet|brev\\adddot\n\
             \patentde|brevet allemand|brev\\adddotspace allem\\adddot\n\
             \patenteu|brevet europ\\'een|brev\\adddotspace europ\\adddot\n\
             \patentuk|brevet britannique|brev\\adddotspace brit\\adddot\n\
             \patentus|brevet am\\'ericain|brev\\adddotspace am\\'er\\adddot\n\
             \patreq|demande de brevet|demande de brev\\adddot\n\
             \patreqde|demande de brevet allemand|demande de brev\\adddotspace allem\\adddot\n\
             \patreqeu|demande de brevet europ\\'een|demande de brev\\adddotspace europ\\adddot\n\
             \patrequk|demande de brevet britannique|demande de brev\\adddotspace brit\\adddot\n\
             \patrequs|demande de brevet am\\'ericain|demande de brev\\adddotspace am\\'er\\adddot\n\
             \file|fichier|fichier\n\
             \library|biblioth\\`eque|biblioth\\adddot\n\
             \abstract|r\\'esum\\'e|r\\'es\\adddot\n\
             \annotation|annotations|annotations\n\
             \commonera|de l'\\`ere commune|EC\n\
             \beforecommonera|avant l'\\`ere commune|AEC\n\
             \annodomini|apr\\`es J\\'esus-Christ|apr\\adddotspace J\\adddot-C\\adddot\n\
             \beforechrist|avant J\\'esus-Christ|av\\adddotspace J\\adddot-C\\adddot\n\
             \circa|vers|vers\n\
             \spring|printemps|printemps\n\
             \summer|\\'et\\'e|\\'et\\'e\n\
             \autumn|automne|automne\n\
             \winter|hiver|hiver\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("galician.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 948)
             "bibliography|Bibliograf\\'ia|Bibliograf\\'ia\n\
             \references|Referencias|Referencias\n\
             \shorthands|Lista de abreviaturas|Abreviaturas\n\
             \editor|editor|ed\\adddot\n\
             \editors|editores|eds\\adddot\n\
             \compiler|compilador|comp\\adddot\n\
             \compilers|compiladores|comp\\adddot\n\
             \redactor|redactor|red\\adddot\n\
             \redactors|redactores|red\\adddot\n\
             \reviser|revisor|rev\\adddot\n\
             \revisers|revisores|revs\\adddot\n\
             \founder|fundador|fund\\adddot\n\
             \founders|fundadores|fund\\adddot\n\
             \continuator|continuador|cont\\adddot\n\
             \continuators|continuadores|cont\\adddot\n\
             \collaborator|colaborador|col\\adddot\n\
             \collaborators|colaboradores|cols\\adddot\n\
             \translator|tradutor|trad\\adddot\n\
             \translators|tradutores|trads\\adddot\n\
             \commentator|comentador|coment\\adddot\n\
             \commentators|comentadores|coments\\adddot\n\
             \annotator|anotador|anot\\adddot\n\
             \annotators|anotadores|anots\\adddot\n\
             \commentary|comentario|coment\\adddot\n\
             \annotations|notas|notas\n\
             \introduction|introduci\\'on|introd\\adddot\n\
             \foreword|prefacio|pref\\adddot\n\
             \afterword|posfacio|posf\\adddot\n"#)),
       ("german.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 1018)
             "bibliography|Literaturverzeichnis|Literatur\n\
             \references|Literaturverzeichnis|Literatur\n\
             \shorthands|Sigelverzeichnis|Sigel\n\
             \editor|Herausgeber|Hrsg\\adddot\n\
             \editors|Herausgeber|Hrsg\\adddot\n\
             \compiler|Kompilator|Komp\\adddot\n\
             \compilers|Kompilatoren|Komp\\adddot\n\
             \redactor|Bearbeiter|Bearb\\adddot\n\
             \redactors|Bearbeiter|Bearb\\adddot\n\
             \reviser|\\\"Uberarbeiter|\\\"Uberarb\\adddot\n\
             \revisers|\\\"Uberarbeiter|\\\"Uberarb\\adddot\n\
             \founder|Begr\\\"under|Begr\\adddot\n\
             \founders|Begr\\\"under|Begr\\adddot\n\
             \continuator|Fortf\\\"uhrer|Fortf\\adddot\n\
             \continuators|Fortf\\\"uhrer|Fortf\\adddot\n\
             \collaborator|Mitarbeiter|Mitarb\\adddot\n\
             \collaborators|Mitarbeiter|Mitarb\\adddot\n\
             \translator|\\\"Ubersetzer|\\\"Ubers\\adddot\n\
             \translators|\\\"Ubersetzer|\\\"Ubers\\adddot\n\
             \commentator|Kommentator|Komm\\adddot\n\
             \commentators|Kommentatoren|Komm\\adddot\n\
             \annotator|Kommentator|Komm\\adddot\n\
             \annotators|Kommentatoren|Komm\\adddot\n\
             \commentary|Kommentar|Komm\\adddot\n\
             \annotations|Erl\\\"auterungen|Erl\\\"aut\\adddot\n\
             \introduction|Einleitung|Einl\\adddot\n\
             \foreword|Vorwort|Vorw\\adddot\n\
             \afterword|Nachwort|Nachw\\adddot\n"#)),
       ("greek.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 469)
             "bibliography|\\206\\146\\206\\185\\206\\178\\206\\187\\206\\185\\206\\191\\206\\179\\207\\129\\206\\177\\207\\134\\206\\175\\206\\177|\\206\\146\\206\\185\\206\\178\\206\\187\\206\\185\\206\\191\\206\\179\\207\\129\\206\\177\\207\\134\\206\\175\\206\\177\n\
             \references|\\206\\145\\206\\189\\206\\177\\207\\134\\206\\191\\207\\129\\206\\173\\207\\130|\\206\\145\\206\\189\\206\\177\\207\\134\\206\\191\\207\\129\\206\\173\\207\\130\n\
             \shorthands|\\206\\154\\206\\177\\207\\132\\206\\172\\206\\187\\206\\191\\206\\179\\206\\191\\207\\130 \\207\\131\\207\\133\\206\\189\\207\\132\\206\\188\\206\\174\\207\\131\\206\\181\\207\\137\\206\\189|\\206\\163\\207\\133\\206\\189\\207\\132\\206\\188\\206\\174\\207\\131\\206\\181\\206\\185\\207\\130\n\
             \editor|\\206\\181\\207\\128\\206\\185\\206\\188\\206\\181\\206\\187\\206\\183\\207\\132\\206\\174\\207\\130|\\206\\181\\207\\128\\206\\185\\206\\188\\206\\181\\206\\187\\206\\183\\207\\132\\206\\174\\207\\130\n\
             \editors|\\206\\181\\207\\128\\206\\185\\206\\188\\206\\181\\206\\187\\206\\183\\207\\132\\206\\173\\207\\130|\\206\\181\\207\\128\\206\\185\\206\\188\\206\\181\\206\\187\\206\\183\\207\\132\\206\\173\\207\\130\n\
             \compiler|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\183\\207\\130|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\183\\207\\130\n\
             \compilers|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\181\\207\\130|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\181\\207\\130\n\
             \redactor|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\183\\207\\130|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\183\\207\\130\n\
             \redactors|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\181\\207\\130|\\207\\131\\207\\133\\206\\189\\207\\132\\206\\172\\206\\186\\207\\132\\206\\181\\207\\130\n"#)),
       ("hungarian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("icelandic.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 250)
             "bibliography|Heimildaskr\\'a|Heimildaskr\\'a\n\
             \references|Heimildir|Heimildir\n\
             \shorthands|Listi yfir skammstafanir|Skammstafanir\n\
             \editor|ristj\\'ori|ritstj\\adddot\n\
             \editors|ritstj\\'orar|ritstj\\adddot\n\
             \compiler|samantekt|samantekt\n\
             \compilers|samantekt|samantekt\n"#)),
       ("italian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 321)
             "bibliography|Bibliografia|Bibliografia\n\
             \references|Riferimenti bibliografici|Riferimenti bibliografici\n\
             \shorthands|Elenco delle sigle|Sigle\n\
             \editor|curatore|cur\\adddot\n\
             \editors|curatori|cur\\adddot\n\
             \compiler|compilatore|comp\\adddot\n\
             \compilers|compilatori|comp\\adddot\n\
             \redactor|redattore|red\\adddot\n\
             \redactors|redattori|red\\adddot\n"#)),
       ("latvian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 2055)
             "redactor|redaktors|redakt\\adddot\n\
             \redactors|redaktori|radakt\\adddot\n\
             \reviser|revidents|revid\\adddot\n\
             \revisers|revidenti|revid\\adddot\n\
             \introduction|ievads|ievads\n\
             \byeditor|izdevis|izdevis\n\
             \bytranslator|tulkojis|tulk\\adddot\n\
             \withintroduction|ar ievadu no|ar ievadu no\n\
             \byeditortr|izdevis un tulkojis|izdev\\adddotspace un tulk\\adddot\n\
             \and|un|un\n\
             \andothers|un citi|u\\adddotspace c\\adddot\n\
             \jourvol|izdevums|izdev\\adddot\n\
             \issue|izdevums|izdev\\adddot\n\
             \edition|izdevums|izdev\\adddot\n\
             \translationof|tulkojums no|tulk\\adddotspace no\n\
             \translationfrom|tulkots no|tulk\\adddotspace no\n\
             \reviewof|recenzija|recenz\\adddot\n\
             \bypublisher|no|no\n\
             \nodate|bez datuma|bez dat\\adddot\n\
             \page|lappuse|lpp\\adddot\n\
             \pages|lappuses|lpp\\adddot\n\
             \column|sleja|sleja\n\
             \columns|slejas|slejas\n\
             \line|rinda|rinda\n\
             \lines|rindas|rindas\n\
             \verse|pants|pants\n\
             \verses|panti|panti\n\
             \paragraph|rindkopa|rindk\\adddot\n\
             \paragraphs|rindkopas|rindk\\adddot\n\
             \pagetotal|lappuse|lpp\\adddot\n\
             \pagetotals|lappuses|lpp\\adddot\n\
             \columntotal|sleja|sleja\n\
             \columntotals|slejas|slejas\n\
             \linetotal|rinda|rinda\n\
             \linetotals|rindas|rindas\n\
             \versetotal|pants|pants\n\
             \versetotals|panti|panti\n\
             \paragraphtotal|rindkopa|rindk\\adddot\n\
             \paragraphtotals|rindkopas|rindk\\adddot\n\
             \in||\n\
             \number|numurs|nr\\adddot\n\
             \datacd|datu CD|datu CD\n\
             \audiocd|audio CD|audio CD\n\
             \version|versija|versija\n\
             \url|pieejams|pieejams\n\
             \urlfrom|pieejams no|pieejams no\n\
             \inpreparation|tiek gatavots|tiek gatavots\n\
             \submitted|iesniegts|iesniegts\n\
             \march|marts|marts\n\
             \may|maijs|maijs\n\
             \august|augusts|aug\\adddot\n\
             \september|septembris|sept\\adddot\n\
             \october|oktobris|okt\\adddot\n\
             \november|novembris|nov\\adddot\n\
             \december|decembris|dec\\adddot\n\
             \langfinnish|somu|somu\n\
             \langrussian|krievu|krievu\n\
             \langswedish|zviedru|zviedru\n\
             \fromfinnish|no somu|no somu\n\
             \fromrussian|no krievu|no krievu\n\
             \fromswedish|no zviedru|no zviedru\n\
             \countryep|Eiropas Parlaments|EP\n\
             \countryfr|Francija|FR\n\
             \patent|patents|patents\n\
             \patenteu|Eiropas patents|Eiropas pat\\adddot\n\
             \patentus|ASV patents|ASV pat\\adddot\n\
             \file|fails|fails\n\
             \circa|ap|ap\n\
             \spring|pavasaris|pav\\adddot\n\
             \summer|vasara|vas\\adddot\n\
             \autumn|rudens|rud\\adddot\n\
             \winter|ziema|ziema\n\
             \am|AM|AM\n\
             \pm|PM|PM\n\
             \dateyear|gada|g\\adddot\n"#)),
       ("lithuanian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 1221)
             "bibliography|Bibliografija|Bibliografija\n\
             \references|Literat\\197\\171ros s\\196\\133ra\\197\\161as|Literat\\197\\171ra\n\
             \shorthands|Santrump\\197\\179 s\\196\\133ra\\197\\161as|Santrumpos\n\
             \editor|sudarytojas|sud\\adddot\n\
             \editors|sudarytojai|sud\\adddot\n\
             \compiler|pareng\\196\\151jas|pareng\\adddot\n\
             \compilers|pareng\\196\\151jai|pareng\\adddot\n\
             \redactor|redaktorius|red\\adddot\n\
             \redactors|redaktoriai|red\\adddot\n\
             \reviser|korektorius|kor\\adddot\n\
             \revisers|korektoriai|kor\\adddot\n\
             \founder|pradininkas|prad\\adddot\n\
             \founders|pradininkai|prad\\adddot\n\
             \continuator|t\\196\\153s\\196\\151jas|t\\196\\153s\\adddot\n\
             \continuators|t\\196\\153s\\196\\151jai|t\\196\\153s\\adddot\n\
             \collaborator|bendradarbis|bendr\\adddot\n\
             \collaborators|bendradarbiai|bendr\\adddot\n\
             \translator|vert\\196\\151jas|vert\\adddot\n\
             \translators|vert\\196\\151jai|vert\\adddot\n\
             \commentator|komentar\\197\\179 autorius|koment\\adddotspace aut\\adddot\n\
             \commentators|komentar\\197\\179 autoriai|koment\\adddotspace aut\\adddot\n\
             \annotator|paai\\197\\161kinim\\197\\179 autorius|paai\\197\\161k\\adddotspace aut\\adddot\n\
             \annotators|paai\\197\\161kinim\\197\\179 autoriai|paai\\197\\161k\\adddotspace aut\\adddot\n\
             \commentary|komentarai|koment\\adddot\n\
             \annotations|paai\\197\\161kinimai|paai\\197\\161k\\adddot\n\
             \introduction|\\196\\175vadas|\\196\\175v\\adddot\n\
             \foreword|pratarm\\196\\151|prat\\adddot\n\
             \afterword|pabaigos \\197\\190odis|pab\\adddotspace \\197\\190odis\n\
             \organizer|organizatorius|org\\adddot\n\
             \organizers|organizatoriai|org\\adddot\n\
             \byorganizer|organizavo|org\\adddot\n"#)),
       ("magyar.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 1070)
             "bibliography|Irodalomjegyz\\'ek|Irodalom\n\
             \references|Hivatkoz\\'asok|Hivatkoz\\'asok\n\
             \shorthands|R\\\"ovid\\'it\\'esek jegyz\\'eke|R\\\"ovid\\'it\\'esek\n\
             \compiler|\\\"ossze\\'all\\'it\\'o|\\\"ossze\\'all\\adddot\n\
             \compilers|\\\"ossze\\'all\\'it\\'ok|\\\"ossze\\'all\\adddot\n\
             \reviser|korrektor|korr\\adddot\n\
             \revisers|korrektorok|korr\\adddot\n\
             \founder|alap\\'it\\'o|alap\\adddot\n\
             \founders|alap\\'it\\'ok|alap\\adddot\n\
             \%continuator||\n\
             \%continuators||\n\
             \translator|ford\\'it\\'o|ford\\adddot\n\
             \translators|ford\\'it\\'ok|ford\\adddot\n\
             \commentator|komment\\'ator|komm\\adddot\n\
             \commentators|komment\\'atorok|komm\\adddot\n\
             \annotator|jegyzetek \\'ir\\'oja|jegyz\\adddot\n\
             \annotators|jegyzetek \\'ir\\'oi|jegyz\\adddot\n\
             \commentary|komment\\'ar|komm\\adddot\n\
             \annotations|jegyzetek|jegyz\\adddot\n\
             \introduction|bevezet\\'es|bev\\adddot\n\
             \afterword|ut\\'osz\\'o|ut\\'osz\\'o\n\
             \byorganizer|szervezte|szerv\\adddot\n\
             \byauthor|\\'irta|\\'irta\n\
             \byeditor|szerkesztette|szerk\\adddot\n\
             \bycompiler|\\\"ossze\\'all\\'itotta|\\\"ossze\\'all\\adddot\n\
             \byredactor|sajt\\'o al\\'a rendezte|kiad\\adddot\n\
             \byreviser|jav\\'itotta|jav\\adddot\n\
             \byreviewer|b\\'ir\\'alta|b\\'ir\\adddot\n\
             \byfounder|alap\\'itotta|alap\\adddot\n"#)),
       ("naustrian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 96)
             "citedas|im Folgenden zitiert als|im Folgenden zit\\adddotspace als\n\
             \january|J\\\"anner|J\\\"an\\adddot\n"#)),
       ("newzealand.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("ngerman.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 66)
             "citedas|im Folgenden zitiert als|im Folgenden zit\\adddotspace als\n"#)),
       ("norsk.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 3252)
             "bibliography|Bibliografi|Bibliografi\n\
             \references|Referanser|Referanser\n\
             \shorthands|Forkortelser|Forkortelser\n\
             \compiler|kompilator|komp\\adddot\n\
             \compilers|kompilatorer|komp\\adddot\n\
             \redactor|bearbeidelse|bearb\\adddot\n\
             \redactors|bearbeidelse|bearb\\adddot\n\
             \reviser|revisjon|rev\\adddot\n\
             \revisers|revisjon|rev\\adddot\n\
             \founder|grunnlegger|grunnl\\adddot\n\
             \founders|grunnleggere|grunnl\\adddot\n\
             \collaborator|samarbeid|samarb\\adddot\n\
             \collaborators|samarbeid|samarb\\adddot\n\
             \translator|oversetter|overs\\adddot\n\
             \translators|oversettere|overs\\adddot\n\
             \commentator|kommentarer|komm\\adddot\n\
             \commentators|kommentarer|komm\\adddot\n\
             \annotator|forklaringer|forkl\\adddot\n\
             \annotators|forklaringer|forkl\\adddot\n\
             \commentary|kommentarer|komm\\adddot\n\
             \annotations|forklaringer|forkl\\adddot\n\
             \introduction|innledning|innl\\adddot\n\
             \foreword|forord|forord\n\
             \afterword|etterord|etterord\n\
             \organizer|organisator|org\\adddot\n\
             \organizers|organisatorer|org\\adddot\n\
             \byorganizer|organisert av|org\\adddotspace av\n\
             \byauthor|av|av\n\
             \byeditor|redigert av|red\\adddotspace av\n\
             \bycompiler|kompilert av|komp\\adddotspace av\n\
             \byredactor|bearbeidet av|bearb\\adddotspace av\n\
             \byreviser|revidert av|rev\\adddotspace av\n\
             \byreviewer|kritikk ved|krit\\adddotspace ved\n\
             \byfounder|grunnlagt av|grunnl\\adddotspace av\n\
             \bycollaborator|i samarbeid med|i samarb\\adddotspace med\n\
             \bytranslator|oversatt \\lbx@lfromlang\\ av|overs\\adddot\\ \\lbx@sfromlang\\ av\n\
             \bycommentator|kommentert av|komm\\adddot\\ av\n\
             \byannotator|forklart av|forkl\\adddot\\ av\n\
             \withcommentator|med kommentarer av|med komm\\adddot\\ av\n\
             \withannotator|med forklaringer av|med forkl\\adddot\\ av\n\
             \withintroduction|med innledning av|med innl\\adddot\\ av\n\
             \withforeword|med forord av|med forord av\n\
             \withafterword|med etterord av|med etterord av\n\
             \and|og|og\n\
             \andothers|med flere|mfl\\adddot\n\
             \andmore|med mer|m.m\\adddot\n\
             \volume|bind|bd\\adddot\n\
             \volumes|bind|bd\\adddot\n\
             \involumes|i|i\n\
             \jourser|serie|ser\\adddot\n\
             \book|bok|bok\n\
             \part|del|del\n\
             \issue|nummer|nr\\adddot\n\
             \newseries|ny serie|ny ser\\adddot\n\
             \oldseries|gammel serie|gl\\adddotspace ser\\adddot\n\
             \edition|utgave|utg\\adddot\n\
             \reprint|opptrykk|opptr\\adddot\n\
             \reprintof|opptrykk av|opptr\\adddotspace av\n\
             \reprintas|trykt om som|tr\\adddotspace om som\n\
             \reprintfrom|opptrykk fra|opptr\\adddotspace fra\n\
             \translationof|oversettelse av|overs\\adddotspace av\n\
             \translationas|oversatt som|overs\\adddotspace som\n\
             \translationfrom|oversatt fra|overs\\adddotspace fra\n\
             \reviewof|kritikk av|krit\\adddotspace av\n\
             \origpubas|opprinnelig utgitt som|oppr\\adddot\\ utg\\adddot\\ som\n\
             \origpubin|opprinnelig utgitt i|oppr\\adddot\\ utg\\adddot\\ i\n\
             \astitle|som|som\n\
             \bypublisher|av|av\n\
             \nodate|udatert|udatert\n\
             \page|side|s\\adddot\n\
             \pages|sider|s\\adddot\n\
             \column|spalte|sp\\adddot\n\
             \columns|spalter|sp\\adddot\n\
             \line|linje|l\\adddot\n\
             \lines|linjer|l\\adddot\n\
             \verse|vers|v\\adddot\n\
             \verses|vers|v\\adddot\n\
             \section|avsnitt|avsn\\adddot\n\
             \sections|avsnitt|avsn\\adddot\n\
             \paragraph|avsnitt|avsn\\adddot\n\
             \paragraphs|avsnitt|avsn\\adddot\n\
             \pagetotal|side|s\\adddot\n\
             \pagetotals|sider|s\\adddot\n\
             \columntotal|spalte|sp\\adddot\n\
             \columntotals|spalter|sp\\adddot\n\
             \linetotal|linje|l\\adddot\n\
             \linetotals|linjer|l\\adddot\n\
             \versetotal|vers|v\\adddot\n\
             \versetotals|vers|v\\adddot\n\
             \sectiontotal|avsnitt|avsn\\adddot\n\
             \sectiontotals|avsnitt|avsn\\adddot\n\
             \paragraphtotal|avsnitt|avsn\\adddot\n\
             \paragraphtotals|avsnitt|avsn\\adddot\n\
             \in|i|i\n\
             \inseries|i|i\n\
             \ofseries|av|av\n\
             \number|nummer|nr\\adddot\n\
             \chapter|kapittel|kap\\adddot\n"#)),
       ("nswissgerman.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 95)
             "citedas|im Folgenden zitiert als|im Folgenden zit\\adddotspace als\n\
             \countryuk|Grossbritannien|GB\n"#)),
       ("nynorsk.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 1126)
             "references|Referansar|Referansar\n\
             \shorthands|Forkortingar|Forkortingar\n\
             \compilers|kompilatorar|komp\\adddot\n\
             \redactor|omarbeiding|omarb\\adddot\n\
             \redactors|omarbeiding|omarb\\adddot\n\
             \founder|grunnleggjar|grunnl\\adddot\n\
             \founders|grunnleggjarar|grunnl\\adddot\n\
             \translator|omsetjar|oms\\adddot\n\
             \translators|omsetjarar|oms\\adddot\n\
             \commentator|kommentarar|komm\\adddot\n\
             \commentators|kommentarar|komm\\adddot\n\
             \annotator|forklaringar|forkl\\adddot\n\
             \annotators|forklaringar|forkl\\adddot\n\
             \commentary|kommentarar|komm\\adddot\n\
             \annotations|forklaringar|forkl\\adddot\n\
             \introduction|innleiing|innl\\adddot\n\
             \organizers|organisatorar|org\\adddot\n\
             \byredactor|omarbeidd av|omarb\\adddotspace av\n\
             \withcommentator|med kommentarar av|med komm\\adddot\\ av\n\
             \withannotator|med forklaringar av|med forkl\\adddot\\ av\n\
             \withintroduction|med innleiing av|med innl\\adddot\\ av\n\
             \andothers|med fleire|mfl\\adddot\n\
             \andmore|med meir|m.m\\adddot\n\
             \oldseries|gamal serie|gl\\adddotspace ser\\adddot\n\
             \translationof|omsetjing av|oms\\adddotspace av\n\
             \translationas|omsett som|oms\\adddotspace som\n\
             \origpubas|opphavleg utgitt som|opph\\adddot\\ utg\\adddot\\ som\n\
             \origpubin|opphavleg utgitt i|opph\\adddot\\ utg\\adddot\\ i\n"#)),
       ("polish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 3661)
             "bibliography|Bibliografia|Bibliografia\n\
             \references|Bibliografia|Bibliografia\n\
             \editor|redaktor|red\\adddot\n\
             \editors|redaktorzy|red\\adddot\n\
             \compiler|redaktor|red\\adddot\n\
             \compilers|redaktorzy|red\\adddot\n\
             \redactor|redaktor|red\\adddot\n\
             \redactors|redaktorzy|red\\adddot\n\
             \reviser|korektor|kor\\adddot\n\
             \revisers|korektorzy|kor\\adddot\n\
             \collaborator|uczestnik|ucz\\adddot\n\
             \collaborators|uczestnicy|ucz\\adddot\n\
             \translator|t\\l umacz|t\\l um\\adddot\n\
             \translators|t\\l umacze|t\\l um\\adddot\n\
             \commentator|komentator|kom\\adddot\n\
             \commentators|komentatorzy|kom\\adddot\n\
             \annotator|uwaga|uw\\adddot\n\
             \annotators|uwagi|uw\\adddot\n\
             \commentary|komentarz|kom\\adddot\n\
             \annotations|komentarze|kom\\adddot\n\
             \foreword|przedmowa|przedm\\adddot\n\
             \afterword|pos\\l owie|pos\\l \\adddot\n\
             \editortr|redaktor i t\\l umacz|red\\adddotspace i per\\adddot\n\
             \editorstr|redaktorzy i t\\l umacze|red\\adddotspace i per\\adddot\n\
             \editorco|redaktor i komentator|red\\adddotspace i kom\\adddot\n\
             \translatorco|t\\l umacz i komentator|t\\l um\\adddotspace i kom\\adddot\n\
             \translatorsco|t\\l umacze i komentatorzy|t\\l um\\adddotspace i kom\\adddot\n\
             \organizer|organizator|org\\adddot\n\
             \organizers|organizatorzy|org\\adddot\n\
             \byorganizer|organizowane|org\\adddot\n\
             \byauthor|autorstwo|aut\\adddot\n\
             \byeditor|redagowa\\l|red\\adddot\n\
             \byredactor|zredagowano|zred\\adddot\n\
             \byreviser|poprawione|popr\\adddot\n\
             \byreviewer|recenzja|rec\\adddot\n\
             \bytranslator|t\\l umaczenie \\lbx@lfromlang|t\\l um\\adddotspace \\lbx@sfromlang\n\
             \bycommentator|skomentowane przez|skom\\adddot\n\
             \byannotator|uwagi od|uw\\adddot\n\
             \withcommentator|komentarze|kom\\adddot\n\
             \withannotator|uwagi|uw\\adddot\n\
             \withforeword|przedmowa|przedm\\adddot\n\
             \withafterword|pos\\l owie|pos\\l \\adddot\n\
             \byeditoran|zredagowany i z uwagami|zredag\\adddotspace i z uw\\adddot\n\
             \byeditoraf|zredagowany, z pos\\l owiem|zred., z pos\\l \\adddot\n\
             \byeditorcofo|zredagowany, komentarze i przedmowa|zred., kom\\adddotspace i przedm\\adddot\n\
             \byeditorcoaf|zredagowany, komentarze i pos\\l owie|zred., kom\\adddotspace i pos\\l \\adddot\n\
             \byeditoranfo|zredagowany, uwagi i przedmowa|zred., uw\\adddotspace i przedm\\adddot\n\
             \byeditoranaf|zredagowany, uwagi i pos\\l owie|zred., uw\\adddotspace i pos\\l \\adddot\n\
             \and|i|i\n\
             \andothers|i inni|i in\\adddot\n\
             \andmore|i inni|i in\\adddot\n\
             \volume|tom|t\\adddot\n\
             \volumes|tomy|t\\adddot\n\
             \involumes|w|w\n\
             \jourvol|tom|t\\adddot\n\
             \jourser|seria|ser\\adddot\n\
             \issue|wydanie|wyd\\adddot\n\
             \newseries|nowa seria|now\\adddotspace ser\\adddot\n\
             \oldseries|stara seria|star\\adddotspace ser\\adddot\n\
             \edition|wydruk|wyd\\adddot\n\
             \reprint|przedruk|przedr\\adddot\n\
             \reprintof|przedruk|przedr\\adddot\n\
             \reprintas|przedrukowano jako|przedr\\adddotspace jako\n\
             \reprintfrom|przedrukowano z|przedr\\adddotspace z\n\
             \translationof|t\\l umaczenie|t\\l um\\adddot\n\
             \translationas|t\\l umaczenie jako|t\\l um\\adddotspace jako\n\
             \translationfrom|t\\l umaczenie z|t\\l um\\adddotspace z\n\
             \reviewof|recenzja na\\addcolon|rec\\adddotspace na\\addcolon\n\
             \origpubin|pierwodruk wydano w|pierw\\adddotspace wyd\\adddotspace w\n\
             \astitle|pod tytu\\l em|pod tyt\\adddot\n\
             \page|strona|s\\adddot\n\
             \pages|strony|s\\adddot\n\
             \column|kolumna|kol\\adddot\n\
             \columns|kolumny|kol\\adddot\n\
             \line|linijka|l\\adddot\n\
             \lines|linijki|l\\adddot\n\
             \nodate|bez daty|b\\adddot d\\adddot\n\
             \verse|werset|wer\\adddot\n\
             \verses|wersety|wer\\adddot\n\
             \section|paragraf|par\\adddot\n\
             \sections|paragrafy|par\\adddot\n\
             \paragraph|akapit|akap\\adddot\n\
             \paragraphs|akapity|akap\\adddot\n\
             \pagetotal|strona|s\\adddot\n\
             \pagetotals|strony|s\\adddot\n\
             \columntotal|kolumna|kol\\adddot\n\
             \columntotals|kolumny|kol\\adddot\n\
             \linetotal|wers|wer\\adddot\n\
             \linetotals|wersy|wer\\adddot\n\
             \versetotal|wiersz|wier\\adddot\n\
             \versetotals|wiersze|wier\\adddot\n\
             \sectiontotal|paragraf|par\\adddot\n\
             \sectiontotals|paragrafy|par\\adddot\n\
             \paragraphtotal|akapit|akap\\adddot\n\
             \paragraphtotals|akapit|akap\\adddot\n\
             \in|w|w\n\
             \inseries|w|w\n\
             \ofseries|z|z\n\
             \number|numer|nr\\adddot\n\
             \chapter|rozdzia\\l |rozd\\adddot\n"#)),
       ("portuges.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("portuguese.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 3318)
             "bibliography|Bibliografia|Bibliografia\n\
             \references|Refer\\^encias|Refer\\^encias\n\
             \shorthands|Lista de Abreviaturas|Abreviaturas\n\
             \editor|editor|ed\\adddot\n\
             \editors|editores|eds\\adddot\n\
             \compiler|compilador|comp\\adddot\n\
             \compilers|compiladores|comp\\adddot\n\
             \redactor|redator|red\\adddot\n\
             \redactors|redatores|red\\adddot\n\
             \reviser|revisor|rev\\adddot\n\
             \revisers|revisores|rev\\adddot\n\
             \founder|fundador|fund\\adddot\n\
             \founders|fundadores|fund\\adddot\n\
             \continuator|continuador|cont\\adddot\n\
             \continuators|continuadores|cont\\adddot\n\
             \collaborator|colaborador|col\\adddot\n\
             \collaborators|colaboradores|col\\adddot\n\
             \translator|tradutor|trad\\adddot\n\
             \translators|tradutores|trad\\adddot\n\
             \commentator|comentador|coment\\adddot\n\
             \commentators|comentadores|coment\\adddot\n\
             \annotator|anotador|anot\\adddot\n\
             \annotators|anotadores|anot\\adddot\n\
             \commentary|coment\\'ario|coment\\adddot\n\
             \annotations|notas|notas\n\
             \foreword|pref\\'acio|pref\\adddot\n\
             \afterword|posf\\'acio|posf\\adddot\n\
             \organizer|organizador|org\\adddot\n\
             \organizers|organizadores|orgs\\adddot\n\
             \byorganizer|organizado por|org\\adddotspace por\n\
             \byauthor|por|por\n\
             \byeditor|editado por|ed\\adddotspace por\n\
             \bycompiler|compilado por|comp\\adddotspace por\n\
             \byredactor|redigido por|red\\adddotspace por\n\
             \byreviser|revisto por|rev\\adddotspace por\n\
             \byreviewer|resenhado por|res\\adddotspace por\n\
             \byfounder|fundado por|fund\\adddotspace por\n\
             \bycontinuator|continuado por|cont\\adddotspace por\n\
             \bytranslator|traduzido \\lbx@lfromlang\\ por|trad\\adddot\\ \\lbx@sfromlang\\ por\n\
             \bycommentator|comentado por|coment\\adddot\\ por\n\
             \byannotator|notas de|notas de\n\
             \withcommentator|com coment\\'arios de|com coment\\adddot\\ de\n\
             \withannotator|com notas de|com notas de\n\
             \withforeword|com pref\\'acio de|com pref\\adddot\\ de\n\
             \withafterword|com posf\\'acio de|com posf\\adddot\\ de\n\
             \and|e|e\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \volume|volume|vol\\adddot\n\
             \volumes|volumes|vols\\adddot\n\
             \involumes|em|em\n\
             \jourvol|volume|vol\\adddot\n\
             \jourser|s\\'erie|s\\'er\\adddot\n\
             \book|livro|livro\n\
             \part|parte|parte\n\
             \issue|n\\'umero|n\\'umero\n\
             \newseries|nova s\\'erie|nova s\\'er\\adddot\n\
             \oldseries|s\\'erie antiga|s\\'er\\adddot\\ antiga\n\
             \reprint|reimpress\\~ao|reimpr\\adddot\n\
             \reprintof|reimpress\\~ao de|reimpr\\adddotspace de\n\
             \reprintas|reimpresso como|reimpr\\adddotspace como\n\
             \reprintfrom|reimpresso|reimpr\\adddot\n\
             \translationas|traduzido como|trad\\adddotspace como\n\
             \translationfrom|traduzido do|trad\\adddotspace do\n\
             \reviewof|resenha de|res\\adddotspace de\n\
             \origpubas|originalmente publicado como|orig\\adddotspace pub\\adddotspace como\n\
             \origpubin|originalmente publicado em|orig\\adddotspace pub\\adddotspace em\n\
             \astitle|como|como\n\
             \bypublisher|por|por\n\
             \nodate|sem data|s\\adddot d\\adddot\n\
             \page|p\\'agina|p\\adddot\n\
             \pages|p\\'aginas|pp\\adddot\n\
             \column|coluna|col\\adddot\n\
             \columns|colunas|cols\\adddot\n\
             \line|linha|l\\adddot\n\
             \lines|linhas|ll\\adddot\n\
             \verse|verso|v\\adddot\n\
             \verses|versos|vv\\adddot\n\
             \paragraph|par\\'agrafo|par\\adddot\n\
             \paragraphs|par\\'agrafo|par\\adddot\n\
             \pagetotal|p\\'agina|p\\adddot\n\
             \pagetotals|p\\'aginas|pp\\adddot\n\
             \columntotal|coluna|col\\adddot\n\
             \columntotals|colunas|cols\\adddot\n\
             \linetotal|linha|l\\adddot\n\
             \linetotals|linhas|ll\\adddot\n\
             \versetotal|verso|v\\adddot\n\
             \versetotals|versos|vv\\adddot\n\
             \paragraphtotal|par\\'agrafo|par\\adddot\n\
             \paragraphtotals|par\\'agrafo|par\\adddot\n\
             \in|em|em\n\
             \inseries|em|em\n\
             \ofseries|de|de\n\
             \number|n\\'umero|n\\adddot\\textordmasculine\n\
             \chapter|cap\\'\\i tulo|cap\\adddot\n"#)),
       ("russian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 4807)
             "bibliography|\\208\\161\\208\\191\\208\\184\\209\\129\\208\\190\\208\\186 \\208\\187\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\209\\139|\\208\\161\\208\\191\\208\\184\\209\\129\\208\\190\\208\\186 \\208\\187\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\209\\139\n\
             \references|\\208\\161\\208\\191\\208\\184\\209\\129\\208\\190\\208\\186 \\208\\187\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\209\\139|\\208\\161\\208\\191\\208\\184\\209\\129\\208\\190\\208\\186 \\208\\187\\208\\184\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\209\\139\n\
             \shorthands|\\208\\161\\208\\191\\208\\184\\209\\129\\208\\190\\208\\186 \\209\\129\\208\\190\\208\\186\\209\\128\\208\\176\\209\\137\\208\\181\\208\\189\\208\\184\\208\\185|\\208\\161\\208\\190\\208\\186\\209\\128\\208\\176\\209\\137\\208\\181\\208\\189\\208\\184\\209\\143\n\
             \editor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \editors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\209\\139|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \compiler|\\209\\129\\208\\190\\209\\129\\209\\130\\208\\176\\208\\178\\208\\184\\209\\130\\208\\181\\208\\187\\209\\140|\\209\\129\\208\\190\\209\\129\\209\\130\\adddot\n\
             \compilers|\\209\\129\\208\\190\\209\\129\\209\\130\\208\\176\\208\\178\\208\\184\\209\\130\\208\\181\\208\\187\\208\\184|\\209\\129\\208\\190\\209\\129\\209\\130\\adddot\n\
             \redactor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \redactors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\209\\139|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \reviser|\\209\\128\\208\\181\\208\\178\\208\\184\\208\\183\\208\\184\\208\\190\\208\\189\\208\\189\\209\\139\\208\\185 \\208\\186\\208\\190\\209\\128\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\178\\208\\184\\208\\183\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \revisers|\\209\\128\\208\\181\\208\\178\\208\\184\\208\\183\\208\\184\\208\\190\\208\\189\\208\\189\\209\\139\\208\\181 \\208\\186\\208\\190\\209\\128\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128\\209\\139|\\209\\128\\208\\181\\208\\178\\208\\184\\208\\183\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \founder|\\208\\190\\209\\129\\208\\189\\208\\190\\208\\178\\208\\176\\209\\130\\208\\181\\208\\187\\209\\140|\\208\\190\\209\\129\\208\\189\\adddot\n\
             \founders|\\208\\190\\209\\129\\208\\189\\208\\190\\208\\178\\208\\176\\209\\130\\208\\181\\208\\187\\208\\184|\\208\\190\\209\\129\\208\\189\\adddot\n\
             \continuator|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\187\\208\\182\\208\\176\\209\\130\\208\\181\\208\\187\\209\\140|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \continuators|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\187\\208\\182\\208\\176\\209\\130\\208\\181\\208\\187\\208\\184|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \collaborator|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\208\\189\\208\\184\\208\\186|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\adddot\n\
             \collaborators|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\208\\189\\208\\184\\208\\186\\208\\184|\\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\adddot\n\
             \translator|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180\\209\\135\\208\\184\\208\\186|\\208\\191\\208\\181\\209\\128\\adddot\n\
             \translators|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180\\209\\135\\208\\184\\208\\186\\208\\184|\\208\\191\\208\\181\\209\\128\\adddot\n\
             \commentator|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \commentators|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128\\209\\139|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotator|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\208\\176\\208\\189\\208\\184\\209\\143|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\adddot\n\
             \annotators|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\208\\176\\208\\189\\208\\184\\209\\143|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\adddot\n\
             \commentary|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128\\208\\184\\208\\185|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotations|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\208\\176\\208\\189\\208\\184\\209\\143|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\adddot\n\
             \introduction|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\208\\184\\209\\130\\208\\181\\208\\187\\209\\140\\208\\189\\208\\176\\209\\143 \\209\\129\\209\\130\\208\\176\\209\\130\\209\\140\\209\\143|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\adddotspace \\209\\129\\209\\130\\adddot\n\
             \foreword|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\184\\209\\129\\208\\187\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\184\\209\\129\\208\\187\\adddot\n\
             \afterword|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\adddot\n\
             \organizer|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\208\\184\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \organizers|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\208\\184\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128\\209\\139|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \byorganizer|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\208\\184\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \byauthor||\n\
             \byeditor|\\208\\191\\208\\190\\208\\180 \\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\134\\208\\184\\208\\181\\208\\185|\\208\\191\\208\\190\\208\\180\\addabbrvspace \\209\\128\\208\\181\\208\\180\\adddot\n\
             \bycompiler|\\209\\129\\208\\190\\209\\129\\209\\130\\208\\176\\208\\178\\208\\184\\209\\130\\208\\181\\208\\187\\209\\140|\\209\\129\\208\\190\\209\\129\\209\\130\\adddot\n\
             \byredactor|\\208\\191\\208\\190\\208\\180 \\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\134\\208\\184\\208\\181\\208\\185|\\208\\191\\208\\190\\208\\180\\addabbrvspace \\209\\128\\208\\181\\208\\180\\adddot\n\
             \byreviser|\\208\\184\\209\\129\\208\\191\\209\\128\\208\\176\\208\\178\\208\\187\\208\\181\\208\\189\\208\\190|\\208\\184\\209\\129\\208\\191\\209\\128\\adddot\n\
             \byreviewer|\\209\\128\\208\\181\\209\\134\\208\\181\\208\\189\\208\\183\\208\\184\\209\\143|\\209\\128\\208\\181\\209\\134\\adddot\n\
             \byfounder|\\209\\131\\209\\135\\209\\128\\208\\181\\208\\180\\208\\184\\209\\130\\208\\181\\208\\187\\209\\140|\\209\\131\\209\\135\\209\\128\\adddot\n\
             \bycontinuator|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\187\\208\\182\\208\\181\\208\\189\\208\\184\\208\\181|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\187\\208\\182\\adddot\n\
             \bycollaborator|\\208\\191\\209\\128\\208\\184 \\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\208\\184\\208\\184|\\208\\191\\209\\128\\208\\184\\addabbrvspace \\209\\131\\209\\135\\adddot\n\
             \bytranslator|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180 \\lbx@lfromlang|\\208\\191\\208\\181\\209\\128\\adddot\\ \\lbx@sfromlang\n\
             \bycommentator|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128\\208\\184\\208\\184|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \byannotator|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\208\\176\\208\\189\\208\\184\\209\\143|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\adddot\n\
             \withcommentator|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128\\208\\184\\208\\184|\\208\\186\\208\\190\\208\\188\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \withannotator|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\208\\176\\208\\189\\208\\184\\209\\143|\\208\\191\\209\\128\\208\\184\\208\\188\\208\\181\\209\\135\\adddot\n\
             \withintroduction|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\208\\184\\209\\130\\208\\181\\208\\187\\209\\140\\208\\189\\208\\176\\209\\143 \\209\\129\\209\\130\\208\\176\\209\\130\\209\\140\\209\\143|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\adddotspace \\209\\129\\209\\130\\adddot\n\
             \withforeword|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\184\\209\\129\\208\\187\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\209\\128\\208\\181\\208\\180\\208\\184\\209\\129\\208\\187\\adddot\n\
             \withafterword|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\208\\190\\209\\129\\208\\187\\208\\181\\209\\129\\208\\187\\adddot\n\
             \and|\\208\\184|\\208\\184\n\
             \andothers|\\208\\184\\addabbrvspace \\208\\180\\209\\128\\adddot|\\208\\184\\addabbrvspace \\208\\180\\209\\128\\adddot\n\
             \andmore|\\208\\184\\addabbrvspace \\208\\180\\209\\128\\adddot|\\208\\184\\addabbrvspace \\208\\180\\209\\128\\adddot\n\
             \volume|\\209\\130\\208\\190\\208\\188|\\209\\130\\adddot\n\
             \volumes|\\209\\130\\208\\190\\208\\188\\208\\176\\209\\133|\\209\\130\\adddot\n\
             \involumes|\\208\\178|\\208\\178\n\
             \jourvol|\\209\\130\\208\\190\\208\\188|\\209\\130\\adddot\n\
             \jourser|\\209\\129\\208\\181\\209\\128\\208\\184\\209\\143|\\209\\129\\208\\181\\209\\128\\adddot\n\
             \book|\\208\\186\\208\\189\\208\\184\\208\\179\\208\\176|\\208\\186\\208\\189\\adddot\n\
             \part|\\209\\135\\208\\176\\209\\129\\209\\130\\209\\140|\\209\\135\\adddot\n\
             \issue|\\208\\178\\209\\139\\208\\191\\209\\131\\209\\129\\208\\186|\\208\\178\\209\\139\\208\\191\\adddot\n\
             \newseries|\\208\\189\\208\\190\\208\\178\\208\\176\\209\\143 \\209\\129\\208\\181\\209\\128\\208\\184\\209\\143|\\208\\189\\208\\190\\208\\178\\adddotspace \\209\\129\\208\\181\\209\\128\\adddot\n\
             \oldseries|\\209\\129\\209\\130\\208\\176\\209\\128\\208\\176\\209\\143 \\209\\129\\208\\181\\209\\128\\208\\184\\209\\143|\\209\\129\\209\\130\\208\\176\\209\\128\\adddotspace \\209\\129\\208\\181\\209\\128\\adddot\n\
             \edition|\\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\184\\208\\181|\\208\\184\\208\\183\\208\\180\\adddot\n\
             \reprint|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\184\\208\\181|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\adddot\n\
             \reprintof|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\184\\208\\181|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\adddot\n\
             \reprintas|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\190 \\208\\186\\208\\176\\208\\186|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\adddotspace \\208\\186\\208\\176\\208\\186\n\
             \reprintfrom|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\190 \\209\\129|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\184\\208\\183\\208\\180\\adddotspace \\209\\129\n\
             \translationof|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180 \\208\\191\\208\\190 \\208\\184\\208\\183\\208\\180\\208\\176\\208\\189\\208\\184\\209\\142\\addcolon|\\208\\191\\208\\181\\209\\128\\adddotspace \\208\\191\\208\\190 \\208\\184\\208\\183\\208\\180\\adddot\\addcolon\n\
             \translationas|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180\\addcolon|\\208\\191\\208\\181\\209\\128\\adddot\\addcolon\n\
             \translationfrom|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\190\\208\\180 \\209\\129|\\208\\191\\208\\181\\209\\128\\adddotspace \\209\\129\n\
             \reviewof|\\209\\128\\208\\181\\209\\134\\208\\181\\208\\189\\208\\183\\208\\184\\209\\143 \\208\\189\\208\\176\\addcolon|\\209\\128\\208\\181\\209\\134\\adddotspace \\208\\189\\208\\176\\addcolon\n\
             \astitle|\\addcolon|\\addcolon\n\
             \bypublisher|\\addcomma|\\addcomma\n\
             \nodate|\\208\\177\\adddot \\208\\179\\adddot|\\208\\177\\adddot \\208\\179\\adddot\n\
             \page|\\209\\129\\209\\130\\209\\128\\208\\176\\208\\189\\208\\184\\209\\134\\208\\176|\\209\\129\\adddot\n\
             \pages|\\209\\129\\209\\130\\209\\128\\208\\176\\208\\189\\208\\184\\209\\134\\209\\139|\\209\\129\\adddot\n\
             \column|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\176|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \columns|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\184|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \line|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176\n\
             \lines|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\184|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176\n\
             \verse|\\209\\129\\209\\130\\208\\184\\209\\133|\\209\\129\\209\\130\\208\\184\\209\\133\n\
             \verses|\\209\\129\\209\\130\\208\\184\\209\\133\\208\\184|\\209\\129\\209\\130\\208\\184\\209\\133\\208\\184\n\
             \section|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132|\\S\n\
             \sections|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132\\209\\139|\\S\\S\n\
             \paragraph|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \paragraphs|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134\\209\\139|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \pagetotal|\\209\\129\\209\\130\\209\\128\\208\\176\\208\\189\\208\\184\\209\\134\\208\\176|\\209\\129\\adddot\n\
             \pagetotals|\\209\\129\\209\\130\\209\\128\\208\\176\\208\\189\\208\\184\\209\\134\\209\\139|\\209\\129\\adddot\n\
             \columntotal|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\176|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \columntotals|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\184|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \linetotal|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176\n\
             \linetotals|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\184|\\209\\129\\209\\130\\209\\128\\208\\190\\208\\186\\208\\176\n\
             \versetotal|\\209\\129\\209\\130\\208\\184\\209\\133|\\209\\129\\209\\130\\208\\184\\209\\133\n\
             \versetotals|\\209\\129\\209\\130\\208\\184\\209\\133\\208\\184|\\209\\129\\209\\130\\208\\184\\209\\133\\208\\184\n\
             \sectiontotal|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132|\\S\n\
             \sectiontotals|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132\\209\\139|\\S\\S\n\
             \paragraphtotal|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \paragraphtotals|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134\\209\\139|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \in|\\208\\178|\\208\\178\n\
             \inseries|\\208\\178|\\208\\178\n\
             \ofseries|\\208\\184\\208\\183|\\208\\184\\208\\183\n\
             \number|\\208\\189\\208\\190\\208\\188\\208\\181\\209\\128|\\226\\132\\150\n\
             \chapter|\\208\\179\\208\\187\\208\\176\\208\\178\\208\\176|\\208\\179\\208\\187\\adddot\n"#)),
       ("serbian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("serbianc.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("slovak.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 2217)
             "references|Referencie|Referencie\n\
             \shorthands|Zoznam skratiek|Skratky\n\
             \editor|editor|ed\\adddot\n\
             \editors|editori|ed\\adddot\n\
             \compilers|zostavovatelia|zost\\adddot\n\
             \redactor|redaktor|red\\adddot\n\
             \redactors|redaktori|red\\adddot\n\
             \reviser|korektor|kor\\adddot\n\
             \revisers|korektori|kor\\adddot\n\
             \founders|zakladatelia|zakl\\adddot\n\
             \translators|prekladatelia|prekl\\adddot\n\
             \foreword|predhovor|predh\\adddot\n\
             \afterword|doslov|dosl\\adddot\n\
             \byauthor||\n\
             \withforeword|s\\addnbspace predhovorom od|s\\addnbspace predh\\adddot\\ od\n\
             \withafterword|s\\addnbspace doslovom od|s\\addnbspace dosl\\adddot\\ od\n\
             \and|a|a\n\
             \andothers|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \andmore|et\\addabbrvspace al\\adddot|et\\addabbrvspace al\\adddot\n\
             \involumes|in|in\n\
             \book|kniha|kniha\n\
             \edition|vydanie|vyd\\adddot\n\
             \reviewof|recenzia|recenz\\adddot\n\
             \translationof|preklad|prekl\\adddot\n\
             \astitle|ako|ako\n\
             \bypublisher||\n\
             \page|strana|s\\adddot\n\
             \pages|strany|s\\adddot\n\
             \line|riadok|r\\adddot\n\
             \lines|riadky|r\\adddot\n\
             \section|sekcia|\\S\n\
             \sections|sekcie|\\S\\S\n\
             \paragraph|odsek|ods\\adddot\n\
             \paragraphs|odseky|ods\\adddot\n\
             \pagetotal|strana|s\\adddot\n\
             \pagetotals|strany|s\\adddot\n\
             \linetotal|riadok|r\\adddot\n\
             \linetotals|riadky|r\\adddot\n\
             \sectiontotal|sekcia|\\S\n\
             \sectiontotals|sekcie|\\S\\S\n\
             \paragraphtotal|odsek|ods\\adddot\n\
             \paragraphtotals|odseky|ods\\adddot\n\
             \in|in|in\n\
             \inseries|in|in\n\
             \ofseries|z|z\n\
             \chapter|kapitola|kap\\adddot\n\
             \datacd|CD-ROM|CD-ROM\n\
             \audiocd|audio CD|audio CD\n\
             \version|verzia|ver\\adddot\n\
             \url|URL|URL\n\
             \idem|idem|idem\n\
             \idemsm|idem|idem\n\
             \idemsf|eadem|eadem\n\
             \idemsn|idem|idem\n\
             \idempm|eidem|eidem\n\
             \idempf|eaedem|eaedem\n\
             \idempn|eadem|eadem\n\
             \idempp|eidem|eidem\n\
             \ibidem|ibidem|ibid\\adddot\n\
             \opcit|op\\adddotspace cit\\adddot|op\\adddotspace cit\\adddot\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddotspace cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|sq\\adddot|sq\\adddot\n\
             \sequentes|sqq\\adddot|sqq\\adddot\n\
             \passim|passim|pass\\adddot\n\
             \march|marec|mar\\adddot\n\
             \august|august|aug\\adddot\n\
             \september|september|sept\\adddot\n\
             \november|november|nov\\adddot\n\
             \december|december|dec\\adddot\n\
             \countryde|Nemecko|DE\n\
             \patent|patent|pat\\adddot\n\
             \patentus|US patent|US pat\\adddot\n\
             \abstract|abstrakt|abst\\adddot\n\
             \annodomini|po Kristovi|po Kr\\adddot\n\
             \beforechrist|pred Kristom|pred Kr\\adddot\n\
             \circa|cirka|ca\\adddot\n\
             \spring|jar|jar\n\
             \summer|leto|leto\n\
             \winter|zima|zima\n\
             \am|AM|AM\n\
             \pm|PM|PM\n"#)),
       ("slovene.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 897)
             "bibliography|Literatura|Literatura\n\
             \references|Literatura|Literatura\n\
             \shorthands|Kratice|Kratice\n\
             \editor|urednik|ur\\adddot\n\
             \editors|uredniki|ur\\adddot\n\
             \compiler|sestavljalec|sest\\adddot\n\
             \compilers|sestavljalci|sest\\adddot\n\
             \redactor|redaktor|redaktor\n\
             \redactors|redaktorji|redaktorji\n\
             \reviser|korektor|korektor\n\
             \revisers|korektorji|korektorji\n\
             \founder|snovalec|snovalec\n\
             \founders|snovalci|snovalci\n\
             \continuator|nadaljevalec|nadaljevalec\n\
             \continuators|nadaljevalci|nadaljevalci\n\
             \collaborator|sodelavec|sod\\adddot\n\
             \collaborators|sodelavci|sod\\adddot\n\
             \translator|prevajalec|prev\\adddot\n\
             \translators|prevajalci|prev\\adddot\n\
             \commentator|komentator|komentator\n\
             \commentators|komentatorji|komentatorji\n\
             \annotator|anotator|anotator\n\
             \annotators|anotatorji|anotatorji\n\
             \commentary|komentar|komentar\n\
             \annotations|opombe|op\\adddot\n\
             \introduction|uvod|uvod\n\
             \foreword|predgovor|predg\\adddot\n\
             \afterword|spremna beseda|spr\\adddotspace b\\adddot\n"#)),
       ("slovenian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 0) ""#)),
       ("spanish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 7015)
             "references|Referencias|Referencias\n\
             \shorthands|Lista de abreviaturas|Abreviaturas\n\
             \editor|edici\\'on|ed\\adddot\n\
             \editors|edici\\'on|eds\\adddot\n\
             \commentator|comentario|com\\adddot\n\
             \commentators|comentarios|coms\\adddot\n\
             \annotator|anotaciones|anot\\adddot\n\
             \annotators|anotaciones|anots\\adddot\n\
             \commentary|comentario|com\\adddot\n\
             \annotations|notas|notas\n\
             \introduction|introducci\\'on|intr\\adddot\n\
             \foreword|pr\\'ologo|pr\\'ol\\adddot\n\
             \organizer|organizaci\\'on|org\\adddot\n\
             \organizers|organizaci\\'on|orgs\\adddot\n\
             \byorganizer|organizado por|org\\adddotspace por\n\
             \byauthor|de|de\n\
             \byeditor|editado por|ed\\adddotspace por\n\
             \bycompiler|compilado por|comp\\adddotspace por\n\
             \byredactor|redacci\\'on de|red\\adddotspace de\n\
             \byreviser|revisado por|rev\\adddotspace por\n\
             \byfounder|fundado por|fund\\adddotspace por\n\
             \bycontinuator|continuado por|cont\\adddotspace por\n\
             \bytranslator|traducido \\lbx@lfromlang\\ por|trad\\adddot \\lbx@sfromlang\\ por\n\
             \bycommentator|comentado por|com\\adddotspace por\n\
             \byannotator|anotado por|anot\\adddotspace por\n\
             \withcommentator|con comentario de|con com\\adddotspace de\n\
             \withannotator|con notas de|con notas de\n\
             \withintroduction|con introduci\\'on de|con intr\\adddotspace de\n\
             \withforeword|con pr\\'ologo de|con pr\\'ol\\adddotspace de\n\
             \and|y|y\n\
             \andothers|y~col\\adddot|y~col\\adddot\n\
             \andmore|et\\adddotspace al\\adddot|et\\adddotspace al\\adddot\n\
             \volume|volumen|vol\\adddot\n\
             \volumes|vol\\'umenes|vols\\adddot\n\
             \involumes|en|en\n\
             \jourvol|volumen|vol\\adddot\n\
             \jourser|\\'epoca|\\'ep\\adddot\n\
             \book|libro|libro\n\
             \part|parte|parte\n\
             \issue|n\\'umero|n\\'um\\adddot\n\
             \newseries|nueva \\'epoca|n\\adddotspace \\'ep\\adddot\n\
             \oldseries|antigua \\'epoca|ant\\adddotspace \\'ep\\adddot\n\
             \edition|edici\\'on|ed\\adddot\n\
             \reprintas|reimpreso como|reimp\\adddotspace como\n\
             \translationas|traducido como|trad\\adddotspace como\n\
             \translationfrom|traducido del|trad\\adddotspace del\n\
             \origpubas|publicado originalmente como|pub\\adddotspace orig\\adddotspace como\n\
             \origpubin|publicado originalmente en|pub\\adddot orig\\adddot en\n\
             \bypublisher|por la editorial|por la ed\\adddot\n\
             \page|p\\'agina|p\\'ag\\adddot\n\
             \pages|p\\'aginas|p\\'ags\\adddot\n\
             \column|columna|col\\adddot\n\
             \columns|columnas|cols\\adddot\n\
             \verse|verso|v\\adddot\n\
             \verses|versos|vv\\adddot\n\
             \sections|secciones|secs\\adddot\n\
             \pagetotal|p\\'agina|p\\'ag\\adddot\n\
             \pagetotals|p\\'aginas|p\\'ags\\adddot\n\
             \columntotal|columna|col\\adddot\n\
             \columntotals|columnas|cols\\adddot\n\
             \versetotal|verso|v\\adddot\n\
             \versetotals|versos|vv\\adddot\n\
             \sectiontotals|secciones|secs\\adddot\n\
             \in|en|en\n\
             \inseries|en|en\n\
             \ofseries|de|de\n\
             \number|n\\'umero|n\\sptext{o\n\
             \chapter|cap\\'itulo|cap\\adddot\n\
             \bathesis|Tesis de licenciatura|Tesis de lic\\adddot\n\
             \phdthesis|Tesis doctoral|Tesis doct\\adddot\n\
             \candthesis|Tesis de candidatura doctoral|Tesis de cand\\adddotspace doct\\adddot\n\
             \techreport|informe t\\'ecnico|inf\\adddotspace t\\'ec\\adddot\n\
             \software|programa|prog\\adddot\n\
             \datacd|disco de datos|CD de datos\n\
             \audiocd|disco de audio|CD de audio\n\
             \version|versi\\'on|ver\\adddot\n\
             \url|direcci\\'on|direcci\\'on\n\
             \urlfrom|disponible desde|disp\\adddotspace desde\n\
             \urlseen|visitado|visitado\n\
             \submitted|enviado|enviado\n\
             \forthcoming|pr\\'oximamente|prox\\adddot\n\
             \inpress|en prensa|en prensa\n\
             \prepublished|previamente publicado|prepublicado\n\
             \citedas|citado en adelante como|cit\\adddotspace en adelante como\n\
             \thiscite|especialmente|esp\\adddot\n\
             \seenote|ver nota|ver n\\adddot\n\
             \quotedin|citado en|cit\\adddotspace en\n\
             \loccit|loc\\adddotspace cit\\adddot|loc\\adddot cit\\adddot\n\
             \confer|cf\\adddot|cf\\adddot\n\
             \sequens|s\\adddot|s\\adddot\n\
             \sequentes|ss\\adddot|ss\\adddot\n\
             \passim|p\\'assim|p\\'assim\n\
             \see|v\\'ease|vid\\adddot\n\
             \seealso|v\\'ease tambi\\'en|vid\\adddotspace tambi\\'en\n\
             \backrefpage|v\\'ease p\\'agina|vid\\adddotspace p\\'ag\\adddot\n\
             \backrefpages|v\\'eanse p\\'aginas|vid\\adddotspace p\\'ags\\adddot\n\
             \january|enero|ene\\adddot\n\
             \february|febrero|feb\\adddot\n\
             \march|marzo|mar\\adddot\n\
             \april|abril|abr\\adddot\n\
             \may|mayo|mayo\n\
             \june|junio|jun\\adddot\n\
             \july|julio|jul\\adddot\n\
             \august|agosto|ago\\adddot\n\
             \september|septiembre|sep\\adddot\n\
             \october|octubre|oct\\adddot\n\
             \november|noviembre|nov\\adddot\n\
             \december|diciembre|dic\\adddot\n\
             \langbrazilian|brasile\\~no|brasile\\~no\n\
             \langbulgarian|b\\'ulgaro|b\\'ulgaro\n\
             \langcroatian|croata|croata\n\
             \langczech|checo|checo\n\
             \langdanish|dan\\'es|dan\\'es\n\
             \langdutch|neerland\\'es|neerland\\'es\n\
             \langenglish|ingl\\'es|ingl\\'es\n\
             \langestonian|estonio|estonio\n\
             \langfrench|franc\\'es|franc\\'es\n\
             \langgalician|gallego|gallego\n\
             \langgerman|alem\\'an|alem\\'an\n\
             \langgreek|griego|griego\n\
             \langhungarian|h\\'ungaro|h\\'ungaro\n\
             \langitalian|italiano|italiano\n\
             \langjapanese|japon\\'es|japon\\'es\n\
             \langlatvian|lituano|lituano\n\
             \langnorwegian|noruego|noruego\n\
             \langpolish|polaco|polaco\n\
             \langportuguese|portugu\\'es|portugu\\'es\n\
             \langrussian|ruso|ruso\n\
             \langslovak|eslovaco|eslovaco\n\
             \langslovene|esloveno|esloveno\n\
             \langspanish|espa\\~nol|espa\\~nol\n\
             \langswedish|sueco|sueco\n\
             \langukrainian|ucraniano|ucraniano\n\
             \frombrazilian|del brasile\\~no|del brasile\\~no\n\
             \frombulgarian|del b\\'ulgaro|del b\\'ulgaro\n\
             \fromcroatian|del croata|del croata\n\
             \fromczech|del checo|del checo\n\
             \fromdanish|del dan\\'es|del dan\\'es\n\
             \fromdutch|del neerland\\'es|del neerland\\'es\n\
             \fromenglish|del ingl\\'es|del ingl\\'es\n\
             \fromestonian|del estonio|del estonio\n\
             \fromfrench|del franc\\'es|del franc\\'es\n\
             \fromgalician|del gallego|del gallego\n\
             \fromgerman|del alem\\'an|del alem\\'an\n\
             \fromgreek|del griego|del griego\n\
             \fromhungarian|del h\\'ungaro|del h\\'ungaro\n\
             \fromitalian|del italiano|del italiano\n\
             \fromjapanese|del japon\\'es|del japon\\'es\n\
             \fromlatvian|del lituano|del lituano\n\
             \fromnorwegian|del noruego|del noruego\n\
             \frompolish|del polaco|del polaco\n\
             \fromportuguese|del portugu\\'es|del portugu\\'es\n\
             \fromrussian|del ruso|del ruso\n\
             \fromslovak|del eslovaco|del eslovaco\n\
             \fromslovene|del esloveno|del esloveno\n\
             \fromspanish|del espa\\~nol|del espa\\~nol\n\
             \fromswedish|del sueco|del sueco\n\
             \fromukrainian|del ucraniano|del ucraniano\n\
             \countryde|Alemania|DE\n\
             \countryeu|Uni\\'on Europea|EU\n\
             \countryep|Uni\\'on Europea|EP\n\
             \countryfr|Francia|FR\n\
             \countryuk|Reino Unido|GB\n\
             \countryus|Estados Unidos|US\n\
             \patent|patente|pat\\adddot\n\
             \patentde|patente alemana|pat\\adddotspace alemana\n\
             \patenteu|patente europea|pat\\adddotspace europea\n\
             \patentfr|patente francesa|pat\\adddotspace francesa\n\
             \patentuk|patente brit\\'anica|pat\\adddotspace brit\\'anica\n\
             \patentus|patente estadounidense|pat\\adddotspace estadounidense\n\
             \patreq|solicitud de patente|sol\\adddotspace de pat\\adddot\n\
             \patreqde|solicitud de patente alemana|sol\\adddotspace de pat\\adddot alemana\n\
             \patreqeu|solicitud de patente europea|sol\\adddotspace de pat\\adddot europea\n\
             \patreqfr|solicitud de patente francesa|sol\\adddotspace de pat\\adddot francesa\n\
             \patrequk|solicitud de patente brit\\'anica|sol\\adddotspace de pat\\adddot brit\\'anica\n\
             \patrequs|solicitud de patente estadounidense|sol\\adddotspace de pat\\adddot estadounidense\n\
             \file|archivo|archivo\n\
             \library|biblioteca|bibl\\adddot\n\
             \abstract|resumen|resumen\n\
             \commonera|era com\\'un|e\\adddotspace c\\adddot\n\
             \beforecommonera|antes de la era com\\'un|a\\adddotspace e\\adddotspace c\\adddot\n\
             \annodomini|despu\\'es de Cristo|d\\adddotspace C\\adddot\n\
             \beforechrist|antes de Cristo|a\\adddotspace C\\adddot\n\
             \circa|circa|ca\\adddot\n\
             \spring|primavera|prim\\adddot\n\
             \summer|verano|ver\\adddot\n\
             \autumn|oto\\~no|ot\\adddot\n\
             \winter|invierno|inv\\adddot\n\
             \am|a\\adddotspace m\\adddot|a\\adddot m\\adddot\n\
             \pm|p\\adddotspace m\\adddot|p\\adddot m\\adddot\n"#)),
       ("swedish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 329)
             "bibliography|Litteraturf\\\"orteckning|Litteratur\n\
             \references|Referenser|Referenser\n\
             \shorthands|F\\\"orkortningar|F\\\"orkortningar\n\
             \editor|utgivare|utg\\adddot\n\
             \editors|utgivare|utg\\adddot\n\
             \compiler|sammanst\\\"allare|sammanst\\adddot\n\
             \compilers|sammanst\\\"allare|sammanst\\adddot\n\
             \redactor|redakt\\\"or|red\\adddot\n\
             \redactors|redakt\\\"orer|red\\adddot\n"#)),
       ("swissgerman.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 29)
             "countryuk|Grossbritannien|GB\n"#)),
       ("turkish.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 5314)
             "references|Kaynaklar|Kaynaklar\n\
             \shorthands|K\\i saltmalar dizini|K\\i saltmalar\n\
             \compiler|derleyen|der\\adddot\n\
             \compilers|derleyenler|der\\adddot\n\
             \redactor|yay\\i na haz\\i rlayan|yay\\adddot\\ haz\\adddot\n\
             \redactors|yay\\i na haz\\i rlayanlar|yay\\adddot\\ haz\\adddot\n\
             \reviser|tashih eden|tashih\\adddot\n\
             \revisers|tashih edenler|tashih\\adddot\n\
             \founder|kurucu|kur\\adddot\n\
             \founders|kurucular|kur\\adddot\n\
             \continuator|tamamlayan|tam\\adddot\n\
             \continuators|tamamlayanlar|tam\\adddot\n\
             \collaborator|ortak|ortak\n\
             \collaborators|ortaklar|ortaklar\n\
             \commentator|yorumlayan|yrm\\adddot\n\
             \commentators|yorumlayanlar|yrm\\adddot\n\
             \commentary|yorum|yrm\\adddot\n\
             \byauthor|yazar|yazar\n\
             \bycompiler|derleyen|der\\adddot\n\
             \byredactor|yay\\i na haz\\i rlayan|yay\\adddot\\ haz\\adddot\n\
             \byreviser|tashih|tashih\n\
             \byfounder|kurucu|kur\\adddot\n\
             \bycontinuator|tamamlayan|tam\\adddot\n\
             \bycommentator|yorumlayan|yrm\\adddot\n\
             \withcommentator|yorumlar\\i yla katk\\i da bulunan|yrm\\adddot\\ kat\\adddot\\ bul\\adddot\n\
             \and|ve|ve\n\
             \volume|cilt|c\\adddot\n\
             \volumes|cilt|c\\adddot\n\
             \jourvol|cilt|c\\adddot\n\
             \jourser|seri|seri\n\
             \book|kitap|kitap\n\
             \part|k\\i s\\i m|k\\i s\\i m\n\
             \issue|say\\i|say\\i\n\
             \newseries|yeni seri|yeni seri\n\
             \oldseries|eski seri|eski seri\n\
             \edition|bask\\i|bs\\adddot\n\
             \reprint|yeni bask\\i s\\i|yeni bask\\i s\\i\n\
             \reprintof|yeni bask\\i s\\i|yeni bask\\i s\\i\n\
             \reprintas|yeni bask\\i s\\i|yeni bask\\i s\\i\n\
             \reprintfrom|yeni bask\\i s\\i|yeni bask\\i s\\i\n\
             \reviewof|derlenen eser|der\\adddot\\ eser\n\
             \origpubas|as\\i l eser|as\\i l eser\n\
             \origpubin|as\\i l eser yay\\i n tarihi|as\\i l eser yay\\i n tarihi\n\
             \bypublisher|yay\\i n evi|yay\\i n evi\n\
             \nodate|tarih yok|t\\adddot y\\adddot\n\
             \page|sayfa|s\\adddot\n\
             \pages|sayfalar|ss\\adddot\n\
             \line|sat\\i r|sat\\adddot\n\
             \lines|sat\\i rlar|sat\\adddot\n\
             \verse|m\\i sra|m\\i s\\adddot\n\
             \verses|m\\i sralar|m\\i s\\adddot\n\
             \paragraph|paragraf|par\\adddot\n\
             \paragraphs|paragraflar|par\\adddot\n\
             \pagetotal|toplam sayfa|s\\adddot\n\
             \pagetotals|toplam sayfalar|ss\\adddot\n\
             \linetotal|toplam sat\\i r|sat\\adddot\n\
             \linetotals|toplam sat\\i rlar|sat\\adddot\n\
             \versetotal|toplam m\\i sra|m\\i s\\adddot\n\
             \versetotals|toplam m\\i sralar|m\\i s\\adddot\n\
             \paragraphtotal|toplam paragraf|par\\adddot\n\
             \paragraphtotals|toplam paragraflar|par\\adddot\n\
             \inseries|serilerde|serilerde\n\
             \ofseries|serilerde|serilerde\n\
             \number|numara|no\\adddot\n\
             \bathesis|lisans tezi|lis\\adddot\\ tezi\n\
             \phdthesis|doktora tezi|dok\\adddot\\ tezi\n\
             \candthesis|aday tezi|aday tezi\n\
             \techreport|teknik rapor|tek\\adddot\\ rap\\adddot\n\
             \software|bilgisayar yaz\\i l\\i m\\i|bilg\\adddot\\ yaz\\adddot\n\
             \datacd|CD|CD\n\
             \audiocd|ses CD'si|ses CD'si\n\
             \version|versiyon|ver\\adddot\n\
             \forthcoming|yak\\i nda|yak\\i nda\n\
             \prepublished|taslak bas\\i m\\i|taslak bas\\i m\\i\n\
             \citedas|at\\i f olarak|at\\i f olarak\n\
             \seenote|nota bak\\i n\\i z|nota bkz\\adddot\n\
             \quotedin|al\\i nt\\i|al\\i nt\\i\n\
             \idem|ayn\\i|ayn\\i\n\
             \idemsm|ayn\\i|ayn\\i\n\
             \idemsf|ayn\\i|ayn\\i\n\
             \idemsn|ayn\\i|ayn\\i\n\
             \idempm|ayn\\i lar\\i|ayn\\i lar\\i\n\
             \idempf|ayn\\i lar\\i|ayn\\i lar\\i\n\
             \idempn|ayn\\i lar\\i|ayn\\i lar\\i\n\
             \idempp|ayn\\i lar\\i|ayn\\i lar\\i\n\
             \loccit|at\\i f yap\\i lan yer|at\\i f yap\\adddot\\ yer\n\
             \sequens|takip eden|takip eden\n\
             \sequentes|takip eden|takip eden\n\
             \passim|rastgele|rastgele\n\
             \see|bak\\i n\\i z|bkz\\adddot\n\
             \seealso|ayr\\i ca bak\\i n\\i z|ayr\\i ca bkz\\adddot\n\
             \january|Ocak|Ocak\n\
             \march|Mart|Mar\\adddot\n\
             \april|Nisan|Nis\\adddot\n\
             \may|May\\i s|May\\adddot\n\
             \june|Haziran|Haz\\adddot\n\
             \july|Temmuz|Tem\\adddot\n\
             \october|Ekim|Ekim\n\
             \november|Kas\\i m|Kas\\adddot\n\
             \december|Aral\\i k|Ara\\adddot\n\
             \langamerican|Amerikanca|Amerikanca\n\
             \langbrazilian|Brezilyanca|Brezilyanca\n\
             \langbulgarian|Bulgarca|Bulgarca\n\
             \langcatalan|Katalanca|Katalanca\n\
             \langdanish|Danimarkanca|Danimarkanca\n\
             \langestonian|Estonca|Estonca\n\
             \langfinnish|Fince|Fince\n\
             \langfrench|Frans\\i zca|Frans\\i zca\n\
             \langgalician|Galce|Galce\n\
             \langgerman|Almanca|Almanca\n\
             \langgreek|Yunanca|Yunanca\n\
             \langhungarian|Macarca|Macarca\n\
             \langjapanese|Japonca|Japonca\n\
             \langlatin|Latince|Latince\n\
             \langlatvian|Letonca|Letonca\n\
             \langlithuanian|Litvanca|Litvanca\n\
             \langpolish|Polonyaca|Polonyaca\n\
             \langportuguese|Portekizce|Portekizce\n\
             \langslovene|Slovence|Slovence\n\
             \langukrainian|Ukraynaca|Ukraynaca\n\
             \fromamerican|Amerikanca'dan|Amerikanca'dan\n\
             \frombrazilian|Brezilyanca'dan|Brezilyanca'dan\n\
             \frombulgarian|Bulgarca'dan|Bulgarca'dan\n\
             \fromcatalan|Katalanca'dan|Katalanca'dan\n\
             \fromdanish|Danimarkanca'dan|Danimarkanca'dan\n\
             \fromestonian|Estonyaca'dan|Estonyaca'dan\n\
             \fromfinnish|Fince'den|Fince'den\n\
             \fromfrench|Frans\\i zca'dan|Frans\\i zca'dan\n\
             \fromgalician|Galyaca'dan|Galyaca'dan\n\
             \fromgerman|Almanca'dan|Almanca'dan\n\
             \fromgreek|Yunanca'dan|Yunanca'dan\n\
             \fromhungarian|Macarca'dan|Macarca'dan\n\
             \fromjapanese|Japonca'dan|Japonca'dan\n\
             \fromlatin|Latince'den|Latince'den\n\
             \fromlatvian|Latvianca'dan|Latvianca'dan\n\
             \fromlithuanian|Litvanca'dan|Litvanca'dan\n\
             \frompolish|Polonyaca'dan|Polonyaca'dan\n\
             \fromportuguese|Portekizce'den|Portekizce'den\n\
             \fromslovene|Slovence'den|Slovence'den\n\
             \fromukrainian|Ukraynaca'dan|Ukraynaca'dan\n\
             \countryde|Almanya|DE\n\
             \countryfr|Fransa|FR\n\
             \patent|patent|pat\\adddot\n\
             \patentde|Alman patenti|Alman pat\\adddot\n\
             \patenteu|Avrupa patenti|Avrupa pat\\adddot\n\
             \patentfr|Frans\\i z patenti|Frans\\i z pat\\adddot\n\
             \patentus|Amerika patenti|Amerika pat\\adddot\n\
             \patreq|patent beklemede|pat\\adddot\\ bek\\adddot\n\
             \patreqde|Alman patenti beklemede|Alman pat\\adddot\\ bek\\adddot\n\
             \patreqeu|Avrupa patenti beklemede|Avrupa pat\\adddot\\ bek\\adddot\n\
             \patreqfr|Frans\\i z patenti beklemede|Frans\\i z pat\\adddot\\ bek\\adddot\n\
             \patrequs|Amerika patenti beklemede|Amerika pat\\adddot\\ bek\\adddot\n\
             \file|dosya|dosya\n\
             \commonera|milattan sonra|MS\n\
             \annodomini|milattan sonra|MS\n\
             \summer|Yaz|Yaz\n\
             \autumn|Sonbahar|Sonbahar\n"#)),
       ("ukrainian.lbx.strings",
        GHC.IO.Unsafe.unsafePerformIO
          ((Data.ByteString.Unsafe.unsafePackAddressLen 4679)
             "bibliography|\\208\\159\\208\\181\\209\\128\\208\\181\\208\\187\\209\\150\\208\\186 \\208\\187\\209\\150\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\184|\\208\\155\\209\\150\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\176\n\
             \references|\\208\\159\\208\\181\\209\\128\\208\\181\\208\\187\\209\\150\\208\\186 \\208\\187\\209\\150\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\184|\\208\\155\\209\\150\\209\\130\\208\\181\\209\\128\\208\\176\\209\\130\\209\\131\\209\\128\\208\\176\n\
             \shorthands|\\208\\159\\208\\181\\209\\128\\208\\181\\208\\187\\209\\150\\208\\186 \\209\\129\\208\\186\\208\\190\\209\\128\\208\\190\\209\\135\\208\\181\\208\\189\\209\\140|\\208\\161\\208\\186\\208\\190\\209\\128\\208\\190\\209\\135\\208\\181\\208\\189\\208\\189\\209\\143\n\
             \editor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \editors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\208\\184|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \compiler|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\208\\176\\209\\135|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\adddot\n\
             \compilers|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\208\\176\\209\\135\\209\\150|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\adddot\n\
             \redactor|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \redactors|\\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\130\\208\\190\\209\\128\\208\\184|\\209\\128\\208\\181\\208\\180\\adddot\n\
             \reviser|\\209\\128\\208\\181\\208\\178\\209\\150\\208\\183\\209\\150\\208\\185\\208\\189\\208\\184\\208\\185 \\208\\186\\208\\190\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128|\\209\\128\\208\\181\\208\\178\\209\\150\\208\\183\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \revisers|\\209\\128\\208\\181\\208\\178\\209\\150\\208\\183\\209\\150\\208\\185\\208\\189\\209\\150 \\208\\186\\208\\190\\209\\128\\208\\181\\208\\186\\209\\130\\208\\190\\209\\128\\208\\184|\\209\\128\\208\\181\\208\\178\\209\\150\\208\\183\\adddotspace \\208\\186\\208\\190\\209\\128\\adddot\n\
             \founder|\\208\\183\\208\\176\\209\\129\\208\\189\\208\\190\\208\\178\\208\\189\\208\\184\\208\\186|\\208\\183\\208\\176\\209\\129\\208\\189\\adddot\n\
             \founders|\\208\\183\\208\\176\\209\\129\\208\\189\\208\\190\\208\\178\\208\\189\\208\\184\\208\\186|\\208\\183\\208\\176\\209\\129\\208\\189\\adddot\n\
             \continuator|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\178\\208\\182\\209\\131\\208\\178\\208\\176\\209\\135|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \continuators|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\178\\208\\182\\209\\131\\208\\178\\208\\176\\209\\135\\209\\150|\\208\\191\\209\\128\\208\\190\\208\\180\\adddot\n\
             \collaborator|\\209\\131\\209\\135\\208\\176\\209\\129\\208\\189\\208\\184\\208\\186|\\209\\131\\209\\135\\208\\176\\209\\129\\adddot\n\
             \collaborators|\\209\\131\\209\\135\\208\\176\\209\\129\\208\\189\\208\\184\\208\\186\\208\\184|\\209\\131\\209\\135\\208\\176\\209\\129\\adddot\n\
             \translator|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180\\208\\176\\209\\135|\\208\\191\\208\\181\\209\\128\\adddot\n\
             \translators|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180\\208\\176\\209\\135\\209\\150|\\208\\191\\208\\181\\209\\128\\adddot\n\
             \commentator|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \commentators|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\130\\208\\190\\209\\128\\208\\184|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotator|\\208\\191\\209\\128\\208\\184\\208\\188\\209\\150\\209\\130\\208\\186\\208\\184|\\208\\191\\209\\128\\208\\184\\208\\188\\adddot\n\
             \annotators|\\208\\191\\209\\128\\208\\184\\208\\188\\209\\150\\209\\130\\208\\186\\208\\184|\\208\\191\\209\\128\\208\\184\\208\\188\\adddot\n\
             \commentary|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \annotations|\\208\\191\\209\\128\\208\\184\\208\\188\\209\\150\\209\\130\\208\\186\\208\\184|\\208\\191\\209\\128\\208\\184\\208\\188\\adddot\n\
             \introduction|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\208\\189\\208\\176 \\209\\129\\209\\130\\208\\176\\209\\130\\209\\130\\209\\143|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\adddotspace \\209\\129\\209\\130\\adddot\n\
             \foreword|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\180\\208\\188\\208\\190\\208\\178\\208\\176|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\180\\208\\188\\adddot\n\
             \afterword|\\208\\191\\209\\150\\209\\129\\208\\187\\209\\143\\208\\188\\208\\190\\208\\178\\208\\176|\\208\\191\\209\\150\\209\\129\\208\\187\\209\\143\\208\\188\\adddot\n\
             \organizer|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\209\\150\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \organizers|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\209\\150\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128\\208\\184|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \byorganizer|\\208\\190\\209\\128\\208\\179\\208\\176\\208\\189\\209\\150\\208\\183\\208\\176\\209\\130\\208\\190\\209\\128|\\208\\190\\209\\128\\208\\179\\adddot\n\
             \byauthor|\\208\\183\\208\\179\\209\\150\\208\\180\\208\\189\\208\\190 \\208\\176\\208\\178\\209\\130\\208\\190\\209\\128\\208\\176|\\208\\183\\208\\179\\209\\150\\208\\180\\208\\189\\adddot \\208\\176\\208\\178\\209\\130\\adddot\n\
             \byeditor|\\208\\183\\208\\176 \\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\134\\209\\150\\209\\148\\209\\142|\\208\\183\\208\\176\\addabbrvspace \\209\\128\\208\\181\\208\\180\\adddot\n\
             \bycompiler|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\208\\176\\209\\135|\\209\\131\\208\\186\\208\\187\\208\\176\\208\\180\\adddot\n\
             \byredactor|\\208\\183\\208\\176 \\209\\128\\208\\181\\208\\180\\208\\176\\208\\186\\209\\134\\209\\150\\209\\148\\209\\142|\\208\\183\\208\\176\\addabbrvspace \\209\\128\\208\\181\\208\\180\\adddot\n\
             \byreviser|\\208\\178\\208\\184\\208\\191\\209\\128\\208\\176\\208\\178\\208\\187\\208\\181\\208\\189\\208\\190|\\208\\178\\208\\184\\208\\191\\209\\128\\adddot\n\
             \byreviewer|\\209\\128\\208\\181\\209\\134\\208\\181\\208\\189\\208\\183\\209\\150\\209\\143|\\209\\128\\208\\181\\209\\134\\adddot\n\
             \byfounder|\\208\\183\\208\\176\\209\\129\\208\\189\\208\\190\\208\\178\\208\\189\\208\\184\\208\\186|\\208\\183\\208\\176\\209\\129\\208\\189\\adddot\n\
             \bycontinuator|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\178\\208\\182\\208\\181\\208\\189\\208\\189\\209\\143|\\208\\191\\209\\128\\208\\190\\208\\180\\208\\190\\208\\178\\208\\182\\adddot\n\
             \bycollaborator|\\208\\183\\208\\176 \\209\\131\\209\\135\\208\\176\\209\\129\\209\\130\\209\\142|\\208\\183\\208\\176\\addabbrvspace \\209\\131\\209\\135\\adddot\n\
             \bytranslator|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180 \\lbx@lfromlang|\\208\\191\\208\\181\\209\\128\\adddot\\ \\lbx@sfromlang\n\
             \bycommentator|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128\\209\\150|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \byannotator|\\208\\191\\209\\128\\208\\184\\208\\188\\209\\150\\209\\130\\208\\186\\208\\184|\\208\\191\\209\\128\\208\\184\\208\\188\\adddot\n\
             \withcommentator|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\208\\176\\209\\128\\209\\150|\\208\\186\\208\\190\\208\\188\\208\\181\\208\\189\\209\\130\\adddot\n\
             \withannotator|\\208\\191\\209\\128\\208\\184\\208\\188\\209\\150\\209\\130\\208\\186\\208\\184|\\208\\191\\209\\128\\208\\184\\208\\188\\adddot\n\
             \withintroduction|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\208\\189\\208\\176 \\209\\129\\209\\130\\208\\176\\209\\130\\209\\130\\209\\143|\\208\\178\\209\\129\\209\\130\\209\\131\\208\\191\\adddotspace \\209\\129\\209\\130\\adddot\n\
             \withforeword|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\180\\208\\188\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\180\\208\\188\\adddot\n\
             \withafterword|\\208\\191\\209\\150\\209\\129\\208\\187\\209\\143\\208\\188\\208\\190\\208\\178\\208\\184\\208\\181|\\208\\191\\209\\150\\209\\129\\208\\187\\209\\143\\208\\188\\adddot\n\
             \and|\\209\\130\\208\\176|\\209\\130\\208\\176\n\
             \andothers|\\209\\130\\208\\176\\addabbrvspace \\209\\150\\208\\189\\adddot|\\209\\130\\208\\176\\addabbrvspace \\209\\150\\208\\189\\adddot\n\
             \andmore|\\209\\130\\208\\176\\addabbrvspace \\209\\150\\208\\189\\adddot|\\209\\130\\208\\176\\addabbrvspace \\209\\150\\208\\189\\adddot\n\
             \volume|\\209\\130\\208\\190\\208\\188|\\209\\130\\adddot\n\
             \volumes|\\209\\130\\208\\190\\208\\188\\208\\176\\209\\133|\\209\\130\\adddot\n\
             \involumes|\\208\\178|\\208\\178\n\
             \jourvol|\\209\\130\\208\\190\\208\\188|\\209\\130\\adddot\n\
             \jourser|\\209\\129\\208\\181\\209\\128\\209\\150\\209\\143|\\209\\129\\208\\181\\209\\128\\adddot\n\
             \book|\\208\\186\\208\\189\\208\\184\\208\\179\\208\\176|\\208\\186\\208\\189\\adddot\n\
             \part|\\209\\135\\208\\176\\209\\129\\209\\130\\208\\184\\208\\189\\208\\176|\\209\\135\\adddot\n\
             \issue|\\208\\178\\208\\184\\208\\191\\209\\131\\209\\129\\208\\186|\\208\\178\\208\\184\\208\\191\\adddot\n\
             \newseries|\\208\\189\\208\\190\\208\\178\\208\\176 \\209\\129\\208\\181\\209\\128\\209\\150\\209\\143|\\208\\189\\208\\190\\208\\178\\adddotspace \\209\\129\\208\\181\\209\\128\\adddot\n\
             \oldseries|\\209\\129\\209\\130\\208\\176\\209\\128\\208\\176 \\209\\129\\208\\181\\209\\128\\209\\150\\209\\143|\\209\\129\\209\\130\\208\\176\\209\\128\\adddotspace \\209\\129\\208\\181\\209\\128\\adddot\n\
             \edition|\\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\189\\209\\143|\\208\\178\\208\\184\\208\\180\\adddot\n\
             \reprint|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\189\\209\\143|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\adddot\n\
             \reprintof|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\189\\209\\143|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\adddot\n\
             \reprintas|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\190 \\209\\143\\208\\186|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\adddotspace \\209\\143\\208\\186\n\
             \reprintfrom|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\190 \\208\\183|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\178\\208\\184\\208\\180\\adddotspace \\208\\183\n\
             \translationof|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180 \\208\\191\\208\\190 \\208\\178\\208\\184\\208\\180\\208\\176\\208\\189\\208\\189\\209\\142\\addcolon|\\208\\191\\208\\181\\209\\128\\adddotspace \\208\\191\\208\\190 \\208\\178\\208\\184\\208\\180\\adddot\\addcolon\n\
             \translationas|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180\\addcolon|\\208\\191\\208\\181\\209\\128\\adddot\\addcolon\n\
             \translationfrom|\\208\\191\\208\\181\\209\\128\\208\\181\\208\\186\\208\\187\\208\\176\\208\\180 \\208\\183|\\208\\191\\208\\181\\209\\128\\adddotspace \\208\\183\n\
             \reviewof|\\209\\128\\208\\181\\209\\134\\208\\181\\208\\189\\208\\183\\209\\150\\209\\143 \\208\\189\\208\\176\\addcolon|\\209\\128\\208\\181\\209\\134\\adddotspace \\208\\189\\208\\176\\addcolon\n\
             \astitle|\\addcolon|\\addcolon\n\
             \bypublisher|\\addcomma|\\addcomma\n\
             \nodate|\\208\\177\\adddot \\209\\128\\adddot|\\208\\177\\adddot \\209\\128\\adddot\n\
             \page|\\209\\129\\209\\130\\208\\190\\209\\128\\209\\150\\208\\189\\208\\186\\208\\176|\\209\\129\\adddot\n\
             \pages|\\209\\129\\209\\130\\208\\190\\209\\128\\209\\150\\208\\189\\208\\186\\208\\184|\\209\\129\\adddot\n\
             \column|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\176|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \columns|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\184|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \line|\\209\\128\\209\\143\\208\\180\\208\\190\\208\\186|\\209\\128\\209\\143\\208\\180\\208\\190\\208\\186\n\
             \lines|\\209\\128\\209\\143\\208\\180\\208\\186\\208\\184|\\209\\128\\209\\143\\208\\180\\208\\186\\208\\184\n\
             \verse|\\208\\178\\209\\150\\209\\128\\209\\136|\\208\\178\\209\\150\\209\\128\\209\\136\n\
             \verses|\\208\\178\\209\\150\\209\\128\\209\\136\\209\\150|\\208\\178\\209\\150\\209\\128\\209\\136\\209\\150\n\
             \section|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132|\\S\n\
             \sections|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132\\208\\184|\\S\\S\n\
             \paragraph|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \paragraphs|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134\\208\\184|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \pagetotal|\\209\\129\\209\\130\\208\\190\\209\\128\\209\\150\\208\\189\\208\\186\\208\\176|\\209\\129\\adddot\n\
             \pagetotals|\\209\\129\\209\\130\\208\\190\\209\\128\\209\\150\\208\\189\\208\\186\\208\\184|\\209\\129\\adddot\n\
             \columntotal|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\176|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \columntotals|\\208\\186\\208\\190\\208\\187\\208\\190\\208\\189\\208\\186\\208\\184|\\208\\186\\208\\190\\208\\187\\adddot\n\
             \linetotal|\\209\\128\\209\\143\\208\\180\\208\\190\\208\\186|\\209\\128\\209\\143\\208\\180\\208\\190\\208\\186\n\
             \linetotals|\\209\\128\\209\\143\\208\\180\\208\\186\\208\\184|\\209\\128\\209\\143\\208\\180\\208\\186\\208\\184\n\
             \versetotal|\\208\\178\\209\\150\\209\\128\\209\\136|\\208\\178\\209\\150\\209\\128\\209\\136\n\
             \versetotals|\\208\\178\\209\\150\\209\\128\\209\\136\\209\\150|\\208\\178\\209\\150\\209\\128\\209\\136\\209\\150\n\
             \sectiontotal|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132|\\S\n\
             \sectiontotals|\\208\\191\\208\\176\\209\\128\\208\\176\\208\\179\\209\\128\\208\\176\\209\\132\\208\\184|\\S\\S\n\
             \paragraphtotal|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \paragraphtotals|\\208\\176\\208\\177\\208\\183\\208\\176\\209\\134\\208\\184|\\208\\176\\208\\177\\208\\183\\adddot\n\
             \in|\\208\\178|\\208\\178\n\
             \inseries|\\208\\178|\\208\\178\n\
             \ofseries|\\209\\150\\208\\183|\\209\\150\\208\\183\n\
             \number|\\208\\189\\208\\190\\208\\188\\208\\181\\209\\128|\\226\\132\\150\n\
             \chapter|\\208\\179\\208\\187\\208\\176\\208\\178\\208\\176|\\208\\179\\208\\187\\adddot\n"#))]

-- biblatex localization keys, from files at
-- http://github.com/plk/biblatex/tree/master/tex/latex/biblatex/lbx
biblatexStringMap :: M.Map Text (M.Map Text (Text, Text))
biblatexStringMap = foldr go mempty biblatexLocalizations
 where
  go (fp, bs) =
    let ls = T.lines $ TE.decodeUtf8 bs
     in case parseLang (toIETF $ T.takeWhile (/= '.') $ T.pack fp) of
          Right lang | length ls > 4
            -> M.insert (langLanguage lang)
                        (toStringMap $ map (T.splitOn "|") ls)
          _ -> id
  toStringMap = foldr go' mempty
  go' [term, x, y] = M.insert term (x, y)
  go' _ = id
