open System.IO

//На основе послед. вещественных чисел получить послед. из их последних цифр. 

//получения послед. из последних цифр чисел изначальной послед.
let NumberResult (numbers: float seq) : int seq =     //послед. целых чисел
    numbers
    |> Seq.map (fun n -> 
        n.ToString()                                 //преобраз. числа в строку
        |> fun str -> str.[str.Length - 1]           //последний символ в строке
        |> string                                    //преобраз. последнего символа в строку
        |> int)                                      //преобраз. строки в число

//функция для получения чисел из ввода
let SeqNumber (input: string) : float seq =          //послед. веществ. чисел
    seq {
        for s in input.Split(' ') do
            let Rep_x = s.Replace(".", ",")
            if Rep_x.Length > 17 then
                ()
            else
                match System.Double.TryParse(Rep_x) with
                | (true, num) -> yield num          //если число корректно, добавляем в послед.
                | _ -> ()
    }

//генерация вещ.числа для послед.
let rec NumberGene (minN: float) (maxN: float) : float =
    let r = System.Random()
    let number = r.NextDouble() * (maxN - minN) + minN

    //если больше 17 символов в числе, то генерируем заново (т.к. если больше, то программа некорректно считает)
    if number.ToString("G17").Length > 17 then
        NumberGene minN maxN
    else
        number

//генерация послед.
let RandomNumbers sizeS (minN: float) (maxN: float) : float seq =
    Seq.init sizeS (fun _ -> NumberGene minN maxN)   //создание послед-и

//получение чисел из файла
let FileNumbers (filePath: string) : float seq =
    seq {
        for line in File.ReadLines(filePath) do        //чтение строк из файла
            for s in line.Split(' ') do
                let Rep_x = s.Replace(".", ",")
                if Rep_x.Length <= 17 then
                    match System.Double.TryParse(Rep_x) with
                    | (true, num) -> 
                        //printfn "Добавлен элемент: %f" num
                        yield num        //добавление в последовательность
                    | _ -> ()
    }

//ввод
let rec input1 () =
    printfn ""
    printfn "Выберите способ ввода и нажмите Enter:"
    printfn "1) Ввод чисел с клавиатуры"
    printfn "2) Генерация случайных чисел"
    printfn "3) Ввод чисел из файла"

    match System.Console.ReadLine() with
    | "1" -> InputKey()
    | "2" -> InputRandom()
    | "3" -> InputFile()
    | _ ->
        printfn "Ошибка! Неверный выбор. Попробуйте снова."
        input1()

//ввод с клавиатуры
and InputKey () =
    printfn ""
    printfn "Введите вещественные числа через пробел и нажмите Enter:"
    let input = System.Console.ReadLine()
    let numbers = SeqNumber input                        //послед. из введенных чисел

    if Seq.isEmpty numbers then
        printfn "Ошибка! Все символы в строке некорректны."
        printfn ""
        input1()
    else
        let numbersResult = NumberResult numbers //послед. из последних цифр
        printfn ""
        printf "Последние вещественных цифры чисел: "
        for n in numbersResult do printf $"{n} "

//ввод генерацией
and InputRandom () =
    let minN = -1000000.0     
    let maxN = 1000000.0      

    let sizeS = InputSize()   

    let numbersRandom = RandomNumbers sizeS minN maxN        //послед. сгенерированных чисел
    let numbersResult = NumberResult numbersRandom           //послед. из последних цифр  

    printfn ""
    printfn "Сгенерированные числа:"
    //для того, чтобы выводились все цифры чисел
    numbersRandom |> Seq.iter (fun num -> printf "%s " (num.ToString("G17"))) 
    printfn ""
    printf "Последние вещественных цифры чисел: "
    for n in numbersResult do printf $"{n} "

//ввод из файла
and InputFile () =
    printfn ""
    printf "Введите путь к файлу: "
    let filePath = System.Console.ReadLine()

    if File.Exists(filePath) then
        let numbers = FileNumbers filePath            //послед. из файла
        if Seq.isEmpty numbers then
            printfn "Ошибка! Все символы в строке некорректны."
            printfn ""
            input1()
        else
            printfn "Полученные числа из файла:"
            //for n in numbers do printfn "Элемент: %f" n 
            for n in numbers do printfn " %f" n 
            let numbersResult = NumberResult numbers         //послед. из последних цифр  
            printfn ""
            printf "Последние вещественные цифры чисел: "
            for n in numbersResult do printf $"{n} "
    else
        printfn "Ошибка! Файл не найден."
        printfn ""
        InputFile()

//ввод кол-ва чисел в послед. для генерации
and InputSize () =
    let rec CorrectSize() =
        printf "Введите количество чисел: "
        let sizeSStr = System.Console.ReadLine()
        match System.Int32.TryParse(sizeSStr) with
        | (true, sizeS) when (sizeS > 0 && sizeS <= 100000) -> 
            sizeS
        | _ ->
            printfn "Ошибка! Некорректное количество. Пожалуйста, введите целое число."
            CorrectSize ()
    CorrectSize()

//--------------------------------------------------------------------

//Послед. содержит символы. Составить из них строку. 

//функция, которая составляет строку из последовательности символов
let StringChar (charS: char seq) : string =
    Seq.fold (fun corr c -> corr + string c) "" charS

//генерация последовательности символов
let RandomChars sizeS =
    let r = System.Random()
    let chars = ['A'..'Z'] @ ['a'..'z'] @ ['0'..'9'] @ ['!'..'/']  //символы
   
    //генерация символов
    seq { for _ in 1 .. sizeS do yield List.item (r.Next(List.length chars)) chars }

//получение размера последовательности символов
let rec inputSize () =
    printf "Введите количество символов: "
    let sizeSStr = System.Console.ReadLine()
    match System.Int32.TryParse(sizeSStr) with
    | (true, sizeS) when (sizeS > 0 && sizeS <= 100000) -> 
        sizeS
    | _ ->
        printfn "Ошибка! Некорректное количество. Пожалуйста, введите целое число."
        inputSize ()

//ввод символов из файла
let rec InputFromFile () =
    printf "Введите путь к файлу: "
    let filePath = System.Console.ReadLine()
    if File.Exists(filePath) then
        let content = File.ReadAllText(filePath)   //чтение всего содержимого файла
        seq { for c in content -> c }              //создание послед. из символов из файла
    else
        printfn "Ошибка! Файл не найден."
        printfn ""
        InputFromFile ()

let rec InputChoice () =
    printfn ""
    printfn "Выберите способ ввода и нажмите Enter:"
    printfn "1) Ввод символов с клавиатуры"
    printfn "2) Генерация случайных символов"
    printfn "3) Ввод символов из файла"

    match System.Console.ReadLine() with
    | "1" -> 
        printf "Введите строку с разными символами и нажмите Enter: "
        let input = System.Console.ReadLine()

        if System.String.IsNullOrEmpty(input) then
            Seq.empty
        else
            seq { for c in input -> c }              //преобраз. строки в последовательность символов
    | "2" -> 
        let sizeS = inputSize()                      //получение размера
        RandomChars sizeS
    | "3" -> 
        InputFromFile ()
    | _ ->
        printfn "Некорректный выбор. Попробуйте еще раз."
        InputChoice () 

let rec input2 () =
    let charS = InputChoice()                        //получение последовательности символов
    printfn "Последовательность символов:"
    let stringResult = StringChar charS 
    Seq.iter (fun i -> printf " %A" i) stringResult
    printfn ""
    printfn ""

    if Seq.isEmpty charS then
        printfn "Пустая составленная строка"
    else
        printfn "Составленная строка: '%s'" stringResult 

//--------------------------------------------------------------------

//Выяснить, есть ли файлы в указанном каталоге. 

//создание послед-и с названиями файлов в указанном каталоге
let Files(filePath: string) =
    seq {
        if Directory.Exists(filePath) then             //если существует каталог
            let files = Directory.GetFiles(filePath)   //получение файлов в каталоге
            //по всем файлам
            for f in files do 
                yield Path.GetFileName(f)              //добавление имя файла с расширением
        else
            yield! Seq.empty                           //пустая последовательность
    }

let input3() =
    printfn "Введите путь к каталогу (c:/) :"
    let filePath = System.Console.ReadLine()
    let files = Files(filePath)

    if Seq.isEmpty files then
        printfn "В указанном каталоге нет файлов."
    else
        printfn "В указанном каталоге найдены файлы:"
        files |> Seq.iter (printfn "%s")

//--------------------------------------------------------------------

//старт
let rec Start () =
    printfn ""
    printfn "Выберите задание и нажмите Enter: "
    printfn "1 задание"
    printfn "2 задание"
    printfn "3 задание"
    printfn "Для выхода введите 'q' и Enter"

    let number = System.Console.ReadLine()

    match number with
    | "1" -> 
        printfn ""
        printfn "----:: Последовательность из цифр, которые являются последними в каждом числе ::----"
        input1()
        Start() 
    | "2" -> 
        printfn ""
        printfn "----:: Строка из символов последовательности ::----"
        input2()
        Start() 
    | "3" -> 
        printfn ""
        printfn "----:: Есть ли файлы в каталоге ::----"
        input3()
        Start() 
    | "q" -> 
        printfn "Выход из программы." 
    | _ -> 
        printfn "Ошибка! Входные данные не являются натуральным числом."  
        printfn ""  
        Start() 

Start() 