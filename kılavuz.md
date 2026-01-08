# Kip Kılavuzu

Merhabalar! Kip, Türkçenin dilbilgisi kurallarını tip sistemiyle birleştiren deneysel bir programlama dilidir. İsmin halleri, ünlü uyumu ve diğer Türkçe morfolojik özellikler, Kip'in tip denetimi sürecinin ayrılmaz bir parçasıdır.

Bu proje, programlama dili tasarımı ile dilbilim arasındaki etkileşimi araştırmak amacıyla geliştirilmiştir. Yani günlük üretim kullanımı hedeflemez; ama denemek, kurcalamak ve fikir yürütmek için bir oyun alanıdır.

> [!NOTE]
> Kip deneyseldir; sürümden sürüme sözdizimi ve davranışlar değişebilir.

## İçindekiler

1. [Temel Kavramlar](#temel-kavramlar)
2. [Veri Tipleri](#veri-tipleri)
3. [Sabit Tanımlama](#sabit-tanımlama)
4. [Fonksiyon Tanımı](#fonksiyon-tanımı)
5. [Örüntü Eşleştirme](#örüntü-eşleştirme)
6. [Fonksiyon Çağrımı](#fonksiyon-çağrımı)
7. [Yerleşik Tipler](#yerleşik-tipler)
8. [Dizgeler](#dizgeler)
9. [Etkili Fonksiyonlar](#etkili-fonksiyonlar)
10. [Modüller ve Yükleme](#modüller-ve-yükleme)
11. [Çokbiçimli Tipler](#çokbiçimli-tipler)
12. [Belirsizlikler ve Çözümleme](#belirsizlikler-ve-çözümleme)
13. [Sözdizimi Özeti](#sözdizimi-özeti)
14. [Derleyici Önbelleği](#derleyici-önbelleği)
15. [Standart Kütüphane](#standart-kütüphane)
16. [Önemli Notlar](#önemli-notlar)

## Temel Kavramlar

### İsmin Halleri

Kip'te fonksiyon argümanlarını, Türkçedeki ismin halleriyle ayırırız. Böylece sıraya değil, eklerin taşıdığı hale bakılır.

| Hal | Türkçe Adı | Ek | Örnek |
|-----|-----------|-----|-------|
| Yalın hal | Nominative | (yok) | `sıfır` |
| -i hali | Accusative | -i, -ı, -u, -ü | `sayıyı` |
| -e hali | Dative | -e, -a | `sayıya` |
| -de hali | Locative | -de, -da, -te, -ta | `listede` |
| -den hali | Ablative | -den, -dan, -ten, -tan | `listeden` |
| Tamlayan eki | Genitive | -in, -ın, -un, -ün | `sayının` |
| -le eki | Instrumental | -le, -la, ile | `sayıyla` |
| Tamlanan eki | Possessive (3s) | -i, -ı, -u, -ü, -si, -sı | `ardılı` |
| Şart kipi | Conditional | -sa, -se | `sıfırsa` |

### Ünlü Uyumu

Kip, Türkçe morfolojik çözümleyici [TRmorph](https://github.com/coltekin/TRmorph)'u kullanarak eklerin doğru formunu otomatik olarak tanır. Örneğin `sayının` ve `listesinin` ifadelerindeki farklı tamlayan ekleri (-nın vs -sinin) aynı dilbilimsel hali temsil eder. Böylece yazım doğal kalır.

### Çok Kelimeli İsimler

Kip'te boşluklu isimler kullanılmaz; bunun yerine kelimeleri tire (-) ile birleştiririz:

```
tam-sayı
işaretli-sayı
öğe-listesi
```

## Veri Tipleri

### Basit Numaralandırma Tipleri

Diğer dillerdeki `enum` yapısına benzer tipleri şöyle tanımlayabilirsiniz. Kısa ve okunaklı bir tanım elde edersiniz:

```
Bir doğruluk ya doğru ya da yanlış olabilir.
```

Burada `doğruluk` tip adı, `doğru` ve `yanlış` ise bu tipin yapkılarıdır (data constructor).

### Yapkı Sözdizimi

Kuralı basit:
- Her yapkı `ya` ile başlar
- Son yapkı `ya da` ile başlayabilir (isteğe bağlı)
- Tanım `olabilir.` ile biter

```
Bir gün
ya pazartesi
ya salı
ya çarşamba
ya perşembe
ya cuma
ya cumartesi
ya pazar
olabilir.
```

### Özyinelemeli (Recursive) Tipler

Tipler kendilerine başvurabilir; bu sayede ağaç, liste gibi yapılara ulaşırız. İlk bakışta uzun görünse de yapısı oldukça düzenlidir.

```
Bir (öğe listesi)
ya boş
ya da bir öğenin bir (öğe listesine) eki
olabilir.
```

Bu tanımda:
- `boş`: Argümansız yapkı (boş ağaç ucu)
- `ek`: Bir `öğe` ve bir `öğe listesi` argümanı alan yapkı

`1'in boşa eki` ifadesinde:
- `boşa`: Argümanın tipi, -e haliyle (-a)
- `eki`: Yapkı adı, tamlanan ekiyle (-i)

### Yapkısız Tipler

Boş tip (Haskell'deki `Void`) şöyle tanımlanır:

```
Bir boşluk var olamaz.
```

## Sabit Tanımlama

Sabitleri `diyelim` yapısıyla tanımlarsınız. Kip burada Türkçe bir cümle gibi okunur:

```
pazartesiye ilk-gün diyelim.
salıya ikinci-gün diyelim.
çarşambaya üçüncü-gün diyelim.
```

Burada dikkat etmeniz gerekenler:
- Tanımlanacak değer **-e hali** alır: `pazartesi**ye**`
- İsim yalın halde verilir: `ilk-gün`
- Sözdizimi Türkçe bir cümle gibidir: "Pazartesiye ilk-gün diyelim."

### Ünsüz Yumuşaması

Türkçedeki ünsüz yumuşaması kuralları geçerlidir; Kip bunu doğal şekilde kabul eder:

```
kitabın kapağına önkapak diyelim.
```

`kitap` + `-ın` → `kitabın` (p → b yumuşaması)

## Fonksiyon Tanımı

### Temel Yapı

```
(argüman1) (argüman2) fonksiyon-adı,
  gövde.
```

### Örnek: Tekli Fonksiyon

```
(bu doğruluğun) tersi,
  bu doğruysa, yanlış,
  yanlışsa, doğrudur.
```

Bu tanımda:
- `(bu doğruluğun)`: Argüman, `bu` adıyla bağlanır (herhangi bir isim kullanılabilir)
- `tersi`: Fonksiyon adı (tamlanan eki alır)
- Gövde: Örüntü eşleştirmesi

### Örnek: İkili Fonksiyon

```
(bu öğe listesiyle) (şu öğe listesinin) birleşimi,
  bu boşsa,
    şu,
  ilkin devama ekiyse,
    ilkin (devamla şunun birleşimine) ekidir.
```

Bu tanımda:
- İlk argüman `-le` eki alır: `bu öğe listesiyle`
- İkinci argüman tamlayan eki alır: `şu öğe listesinin`
- Fonksiyon adı tamlanan eki alır: `birleşimi`

## Örüntü Eşleştirme

### Temel Sözdizimi

Örüntü eşleştirmesi şart kipi (-sa/-se) ile yapılır. Okuması da yazması da doğal bir akış verir:

```
değer yapkıysa,
  sonuç,
...
```

### Bağlayıcılar (Binders)

Yapkı argümanlarını bir isme bağlayabilirsiniz:

```
(bu öğe listesinin) kopyası,
  bu boşsa,
    boş,
  ilkin devama ekiyse,
    ilkin (devamın kopyasıyla) birleşimidir.
```

`ilkin devama ekiyse` ifadesinde `ilk` ve `devam`, yapkı argümanlarını bağlar. Böylece gövde içinde bu isimleri rahatça kullanırsınız.

Bağlayıcı isimleri aynı örüntü dalında tekrar edilemez; bu, kafa karışıklığını önler.

### Tireli Bağlayıcılar

Çakışmaları önlemek için tireli isimler kullanılabilir:

```
bu-öncülün ardılıysa,
  bu-öncülün ardılıdır.
```

### İç İçe Örüntü Eşleştirmesi

Örüntü eşleştirmeleri iç içe kullanılabilir:

```
(bu işaretin) çift-öncülü,
  bu boşsa,
    boş,
  bu-öncülün ardılıysa,
    (bu-öncül boşsa,
      boş,
     bu-öncül-öncülün ardılıysa,
      bu-öncül-öncülün ardılıdır).
```

### Değilse (Wildcard)

Kalan tüm olasılıkları kapsamak için `değilse` kullanılır:

```
(bu doğruluğun) tersi,
  bu doğruysa, yanlış,
  değilse, doğru.
```

### Eşleşme İfadesi

Eşleştirme bir ifade olarak da kullanılabilir ve denetlenen ifade basit bir değişken olmak zorunda değildir. Bu, dönüşümleri daha akıcı yazmanıza yardımcı olur:

```
(bu dizgenin) sayının-bir-fazlası,
  ((bunun tam-sayı-hali)
    yokluksa, 0,
    n'nin varlığıysa, (n'nin 1'le toplamıdır)).
```

### Yerleşik Tiplerle Eşleşme

Tam-sayılar gibi yerleşik (primitive) tipler yapıcılarla eşleştirilemez; yalnızca kendi tiplerinin yapıcılarıyla eşleştirme yapılır.

## Fonksiyon Çağrımı

### Temel Çağrı

```
(ikiyle üçün toplamını) yaz.
```

### Esnek Argüman Sırası

Kip'in hoş özelliklerinden biri, argüman sırasının esnek olmasıdır. Aşağıdaki iki çağrı eşdeğerdir:

```
(5'le 3'ün farkını) yaz.
(3'ün 5'le farkını) yaz.
```

Bu esneklik, **argümanların farklı haller alması** sayesinde mümkündür. Kip, hal eklerine bakarak hangi argümanın hangisi olduğunu belirler.

> [!IMPORTANT]
> Aynı hali alan birden fazla argüman varsa, sıralama önemli olur.

### Emir Kipi ile Çağrı

Mastar (-mak/-mek) biçiminde tanımlanan etkili fonksiyonlar, emir kipinde çağrılabilir. Böylece çağrılar daha doğal görünür:

```
selamlamak,
  isim olarak okuyup,
  ("Merhaba "yla ismin birleşimini) yazmaktır.

selamla.
```

## Yerleşik Tipler

### Tam Sayılar

```
Bir yerleşik tam-sayı olsun.
```

Tam sayı işlemleri:

```
(bu tam-sayıyla) (şu tam-sayının) toplamı, yerleşiktir.
(bu tam-sayıyla) (şu tam-sayının) farkı, yerleşiktir.
(bu tam-sayıyla) (şu tam-sayının) çarpımı, yerleşiktir.
(bu tam-sayının) öncülü, yerleşiktir.
(bu tam-sayının) sıfırlığı, yerleşiktir.
(bu tam-sayıyla) (şu tam-sayının) eşitliği, yerleşiktir.
(bu tam-sayının) (şu tam-sayıdan) küçüklüğü, yerleşiktir.
(bu tam-sayının) (şu tam-sayıdan) küçük-eşitliği, yerleşiktir.
(bu tam-sayının) (şu tam-sayıdan) büyüklüğü, yerleşiktir.
(bu tam-sayıyla) (şu tam-sayının) büyük-eşitliği, yerleşiktir.
(bu tam-sayının) faktöriyeli, yerleşiktir.
```

### Tam Sayı Sabitleri

Sayı sabitleri doğrudan kullanılabilir, ek alırken kesme işareti kullanılır:

```
5'i yaz.
(5'le 3'ün toplamını) yaz.
-1'i yaz.
```

## Dizgeler

```
Bir yerleşik dizge olsun.
```

Dizge işlemleri:

```
(bu dizgenin) uzunluğu, yerleşiktir.
(bu dizgeyle) (şu dizgenin) birleşimi, yerleşiktir.
(bu dizgenin) tam-sayı-hali, yerleşiktir.
```

`tam-sayı-hali` dönüşümünde başarısızlık olasılığı vardır; sonuç bir `olasılık` değeridir.

### Dizge Sabitleri

```
"merhaba"'yı yaz.
"merhaba"'yla "dünya"'nın birleşimini yaz.
```

### Kaçış Dizileri

Dizgelerde yaygın kaçış dizileri kullanılabilir:

```
"a\nb\tc\\\"d"'yi yaz.
```

## Etkili Fonksiyonlar

### Yazdırma

```
(bu şeyi) yazmak, yerleşiktir.
(bu dizgeyi) yazmak, yerleşiktir.

5'i yaz.
"merhaba"'yı yaz.
```

### Okuma

```
okumak, yerleşiktir.
```

Standart girdiden bir satır okur ve dizge olarak döner. REPL'de denemek için idealdir.

### Dosya G/Ç

```
(bu dosyadan) (okumak dizge olasılığı), yerleşiktir.
(bu dosyaya) (şu dizgeyi) (yazmak doğruluğu), yerleşiktir.
```

Dosyadan okuma, `olasılık` tipi döndürür:

```
çalıştırmak,
  ("./tests/yazı.tmp"'den okumak)
    yokluksa,
      "Okunamadı." yazmaktır,
    metnin varlığıysa,
      metni yazmaktır.

çalıştır.
```

### Sıralama (Sequencing)

`-ip/-ıp/-up/-üp` zarf-fiil ekleriyle birden fazla işlem sıralanabilir:

```
(bu tam-sayıyı) denemek,
  bunu yazıp,
  (bunla 1'in toplamını) yazmaktır.

5'i dene.
```

### Bağlama (Binding)

`olarak` ile bir ifadenin sonucunu isme bağlayabilirsiniz:

```
selamlamak,
  isim olarak okuyup,
  ("Merhaba "yla ismin birleşimini) yazmaktır.

selamla.
```

Bu örnekte:
1. `okumak` fonksiyonu çağrılır
2. Sonuç `isim` değişkenine bağlanır
3. `isim` sonraki ifadede kullanılır

### Bitim Tipi

Etkili işlemler genellikle `bitim` (OCaml'daki `unit`, C türevi dillerdeki `void`) türünü döndürür. `durmak` bu türün basit bir değeridir:

```
(durmak bitimi),
  bitimliktir.
```

## Modüller ve Yükleme

Kip dosyaları `yükle` ile içe aktarılır. `yükle` komutu -i hali ister (ör. `girişi yükle.`). Varsayılan olarak `lib/giriş.kip` otomatik yüklenir; bu sayede temel modüllerle başlamış olursunuz. İsterseniz bunu `--no-prelude` ile kapatabilirsiniz.

> [!IMPORTANT]
> Otomatik yükleme açıksa, temel modülleri ayrıca çağırmanıza gerek yoktur.

Manuel yüklemek isterseniz:

```
girişi yükle.
```

## Çokbiçimli Tipler

### Tip Değişkenleri

Çokbiçimli (polymorphic) tipler tanımlanabilir:

```
Bir (öğe listesi)
ya boş
ya da bir öğenin bir öğe listesine eki
olabilir.
```

`öğe` burada bir tip değişkenidir. Bu tip, `tam-sayı listesi`, `dizge listesi`, `doğruluk listesi` gibi somutlaştırılabilir.

### Liste Oluşturma

```
doğrunun (yanlışın boşa ekine) ekini yaz.
```

Bu ifade diğer dillerdeki `[true, false]` listesi gibi düşünülebilir.

### Liste Fonksiyonları

```
(bu öğe listesinin) uzunluğu,
  bu boşsa,
    0,
  ilkin devama ekiyse,
    (devamın uzunluğuyla) 1'in toplamıdır.

(bu öğe listesiyle) (şu öğe listesinin) birleşimi,
  bu boşsa,
    şu,
  ilkin devama ekiyse,
    ilkin (devamla şunun birleşimine) ekidir.

(bu öğe listesinin) tersi,
  bu boşsa,
    boş,
  ilkin devama ekiyse,
    (devamın tersiyle) (ilkin boşa ekinin) birleşimidir.

(bu tam-sayı listesinin) toplamı,
  bu boşsa,
    0,
  ilkin devama ekiyse,
    ilkin (devamın toplamıyla) toplamıdır.
```

### Olasılık Tipi (Maybe)

```
Bir (öğenin olasılığı)
ya yokluğu
ya da bir öğenin varlığı olabilir.
```

`yokluk` ve `varlık` üzerinden eşleşme yaparak güvenli dönüşümler yazabilirsiniz:

```
(bu dizgenin) sayıya-çevrimi,
  ((bunun tam-sayı-hali)
    yokluksa, yokluğudur,
    n'nin varlığıysa, n'nin varlığıdır).
```

## Belirsizlikler ve Çözümleme

### Morfolojik Belirsizlik

Türkçede bazı kelimeler birden fazla şekilde ayrıştırılabilir. Örneğin "takası":
- `taka` + kaynaştırma harfi + tamlanan eki
- `takas` + -i hali

Kip bu tür durumlarda tüm olasılıkları saklar ve tip denetimi sırasında bağlama göre doğru olanı seçer.

> [!TIP]
> Belirsizliklerden ötürü Kip bir ifadenin türünü çözemezse, ama ifadenin geçerli olması gerektiğini düşünüyorsanız, kesme işaretiyle niyetinizi açıkça yazabilirsiniz.

### Kesme İşareti ile Ayrım

Belirsizlikleri gidermek için kesme işareti kullanılabilir:

```
taka'sı   (* taka'nın tamlananı *)
takas'ı   (* takası -i haliyle *)
```

### Fonksiyon Aşırı Yüklemesi (Overloading)

Aynı isimde farklı tipli fonksiyonlar tanımlanabilir:

```
(bu tam-sayıyla) (şu tam-sayının) birleşimi,
  (bu tam-sayıyla) (şu tam-sayının) toplamıdır.

(bu doğrulukla) (şu doğruluğun) birleşimi,
  bu doğruysa,
    doğru,
  yanlışsa,
    şudur.

(5'le 2'nin birleşimini) yaz.           (* 7 *)
(doğruyla yanlışın birleşimini) yaz.    (* doğru *)
```

Kip, argüman tiplerine göre doğru fonksiyonu seçer. Okunabilirliği korumak için dönüş tipi de isme dahil edilebilir.

Standart kütüphanede bazı fonksiyon adları, dönüş tipini de içerir (ör. `toplam tam-sayısı`, `uzunluk tam-sayısı`). Bu, aşırı yükleme ve okunabilirlik için kullanılır.

## Sözdizimi Özeti

### Yorum

```
(* Bu bir yorumdur *)
```

### Yükleme

```
modül-adını yükle.
```

### Tip Tanımı

```
Bir tip-adı ya yapkı1 ya yapkı2 ya da yapkı3 olabilir.
Bir tip-adı ya yapkı1 ya da bir argüman-tipinin yapkı2 olabilir.
Bir yerleşik tip-adı olsun.
Bir boş-tip var olamaz.
```

### Sabit Tanımı

```
değere isim diyelim.
```

### Fonksiyon Tanımı

```
(argüman1) fonksiyon-adı,
  gövde.

(argüman1) (argüman2) fonksiyon-adı,
  gövde.

fonksiyon-adı, yerleşiktir.
```

### Örüntü Eşleştirmesi

```
değer yapkıysa, sonuç, değilse, varsayılan-sonuç.
değer yapkı1ysa, sonuç1, yapkı2ysa, sonuç2.
değer bağlayıcının yapkısıysa, bağlayıcıyı-kullanan-sonuç.
```

### Etkili İfadeler

```
ifade1ip, ifade2.
isim olarak ifadeyi, ismi-kullanan-ifade.
değeri yaz.
```

## Derleyici Önbelleği

Kip, her `.kip` dosyasının ayrıştırılmış ve tip denetlenmiş halini aynı dizinde `.iz` uzantılı bir önbelleğe kaydeder. (Python bytecode gibi düşünülebilir.) Kaynak dosya ve onun `yükle` ile çağırdığı bağımlılıklar değişmediyse, Kip bu `.iz` dosyasını kullanır.

`.iz` dosyasında derleyicinin ve dosyanın "hash"i de saklanır; böylece derleyici değiştiğinde önbellek otomatik olarak geçersiz sayılır. Yeniden ayrıştırma ve tip denetimi yapmak isterseniz, ilgili `.iz` dosyasını silmeniz yeterlidir.

> [!NOTE]
> `kip --build` komutu, `.iz` önbelleklerini önceden üretmenize yardımcı olur.

## Standart Kütüphane

`lib/temel.kip` dosyası temel tipleri içerir; standart fonksiyonlar ilgili modüllerdedir:

- Temel tipler: `doğruluk`, `boşluk`, `olasılık`, `liste`, `yerleşik tam-sayı`, `yerleşik dizge`
- Ek temel tip: `bitim`

- `temel-doğruluk.kip`: `tersi`, `birleşimi`, `kesişimi`
- `temel-tam-sayı.kip`: `toplamı`, `farkı`, `çarpımı`, `eşitliği`, `küçüklüğü`, `küçük-eşitliği`, `büyüklüğü`, `büyük-eşitliği`, `öncülü`, `sıfırlığı`, `faktöriyeli`
- `temel-dizge.kip`: `uzunluğu`, `birleşimi`, `tam-sayı-hali` (sonuç: `olasılık`)
- `temel-liste.kip`: `uzunluğu`, `birleşimi`, `tersi`, `toplamı` (tam-sayı listesi)
- `temel-etki.kip`: `durmak`, `yazmak` (şey/dizge), `okumak`, dosyadan `okumak`, dosyaya `yazmak`

## Önemli Notlar

1. Kip satırbaşlarını ve girintileri önemsemez; istediğiniz gibi biçimlendirebilirsiniz.
2. Tek harfli isimlerde tırnak işareti gerekir: `a'nın`, `b'ye`
3. Fonksiyonlar ve yapkılar -dır/-dir ile biter (isteğe bağlı ama önerilir)
4. Bu bir araştırma projesidir; üretim ortamında kullanım için tasarlanmamıştır.

> [!CAUTION]
> Deneysel olduğu için, Kip'i uzun vadeli projelerde kullanmanızı önermiyoruz.