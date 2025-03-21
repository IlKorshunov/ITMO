# hotelbusiness

Здравствуйте, Дональд Крямп!

Ваша империя ширится, и теперь в вашем владении премиальный курорт "Koza Hutor: Resort, Spa, Golang 5*".
Поздравляем с успешным приобретением!

    Правда, есть нюанс.

    Ваш твиттер настолько заряжен энергией на успех и богатство, что предыдущий управляющий курорта сбежал,
    едва узнав, что вы собственной персоной планируете посетить курорт!

    Ваш верный comrade Эдвард Санден немного разобрался в IT-системах курорта и выяснил,
    что курорт интегрирован в международный сервис "Gluking.com" и что к вам уже вот-вот нагрянут гости!

    Эдварду удалось выгрузить информацию о заездах и выездах будущих гостей -- см. структуру `hotelbusiness.Guest`.
    По техническим причинам все даты заменены на цифры. Эдвард говорит, что ноль -- значит сегодня.

    Менеджер по работе с гостями (его зовут Валентин) утверждает, сбежавший управляющий (его имя еще не установлено)
    всегда передавал ему информацию о гостях в другом виде. А именно -- см. структуру `hotelbusiness.Load` --
    указание, с какой даты сколько гостей ожидается на курорте.

    В таком виде Валентин может рассчитать необходимые закупки продуктов, график работы персонала и другие мелочи,
    чтобы курорт функционировал в штатном режиме.

    Эдвард бы рад рассчитать данные для Валентина в требуемом виде, но, говорит, интернет в Шереметьево так себе.

    Давайте покажем всем завистникам вашу мощь и талант! Напишите функцию `hotelbusiness.ComputeLoad`,
    покажите всему твиттеру, что еще есть порох в пороховницах!

    ### Технические уточнения от Валентина

    * Если в один день гости и выезжают, и заезжают, то Валентину важно знать количество гостей к ужину: то есть когда все выезжающие выехали, а все заезжающие заехали.
    * Для упрощения работы Валентин просит сообщать ему только о датах, когда изменяется загрузка курорта.

### Примеры

Как запустить все тесты:
```
go test -v ./hotelbusiness/...
```
