# Fortran-Programming-Exercises
Fortran Programming Exercises that i made in SPB PU
<!DOCTYPE html>
<html>
 <head>
  <meta charset="utf-8">
  <link href=”style.css” rel=”stylesheet”>
  <title></title>
  <style>
  hr {
    border: none; /* Убираем границу */
    background-color: gray; /* Цвет линии */
    height: 2px; /* Толщина линии */
    }
  p1 {
    margin-left: 40px;
  }
  h3{
    text-align: center;
    margin-top: 20px;
    margin-bottom: 10px;
  }

  </style>
 </head>
 <body>
  <img src="https://raw.githubusercontent.com/VAsiaa/Fortran-Programming-Exercises/main/Pics/warning.png" width=20 alt="WARNIONG!"> <p1>Данный материал может не являтся 100% достоверным источником, поскольку помимо материалла данного мне на лекционных занятиях тут присутствует мое личное понимание этих тем, что может не соответствовать истине!</p1> <img src="https://raw.githubusercontent.com/VAsiaa/Fortran-Programming-Exercises/main/Pics/warning.png" width=20 alt="WARNIONG!">

  <p1>Страница находиться на стадии разработки<p1>

  <h1>Практические задания по предмету "Алгоритмизация и программирование"</h1>
  <p1>Данный предмет был направлени  на освоение созданной инструментальной среды разработки и изучения работы со стандартными стуктурами данных. Основной задачей данного предмета является приобретение студентом необходимых знаний и навыков для эффективного написания алгоритмов</p1>

  <p1>Для выполнения поставленных задач была создана следующая инструментальная платформа:</p1>
  <ul>
      <li>Операционная система (Дистрибутив) - Debian GNU/Linux 11 - Official amd64 </li>
      <li>Менеджер окон - i3</li>
      <li>Тектовый редактор - Vim</li>
      <li>Компилятор - GFortran</li>
   </ul>
     <p1>Во всех проектах была использованна следующая структура программы:</p1>

  <ol>
    <li>Ввод данных</li>
    <li>Вывод данных</li>
    <li>Обработка данных</li>
    <li>Вывод обработанных данных</li>
  </ol>
  <h3>Немного теории</h3>
  <p1>Эта краткая выдержка была сделана из многостраничного конспекта</p1>


  <h2>Метод пошаговой детализации</h2>
  <p1><u>Метод пошаговой детализации </u> - реализует нисходящий подход к программированию и предполагает пошаговую разработку алгоритма.</p1>

  <p1>С использованием метода пошаговой детализации разработку алгоритмов выполняют поэтапно. На первом этапе описывают решение поставленной задачи «по крупному», выделяя подзадачи, которые необходимо решить. На следующем – аналогично описывают решение подзадач, формулируя уже подзадачи следующего уровня. Процесс продолжают, пока не доходят до подзадач, алгоритмы решения которых очевидны. При этом, описывая решение каждой задачи, желательно использовать не более одной-двух конструкций, таких как цикл или ветвление, чтобы четче представить себе структуру программы.</p1>

  <h2>Показатели эффективности кода</h2>

  <p1><u>Для определения эффективносмти кода</u> введем следующие относительные показатели</p1>

  <ul>
      <li>Prod (code productivity) - эффективность кода</li>
      <li>Perf (code performance) - производительность кода</li>
      <li>Compl (code complexity) - Сложность кода</li>
      <li>T - время выполнения участнка кода (мс)</li>
  </ul>

  <center><p1>Compl = (Число строк, Цикломатическая сложность)</p1></center>

  <center><p1>Perf = 1/t </p1></center>

  <center><p1>Prod = perf/Compl </p1></center>

  <h2>Регулярный доступ к памяти </h2>
  <p1><u>Регулярный доступ к памяти  </u> - осуществляется перебором элементов массива по порядку (перебор элементов массива или иной другой структуры последовательно так как элементы распологаются в оперативной памяти).</p1>

  <p1>В оперативной памяти двумерные массивы распологаются следующим образом: </p1>
  <center><img src="https://raw.githubusercontent.com/VAsiaa/Fortran-Programming-Exercises/main/Pics/pic2.png" alt="Расположение двумерного массива в оперативной памяти"></center>

  <p1>Очень важно работать с данными используя регуляриный доступ к памяти, поскольку такой подход исколючает "Хеш-промахи" и позволяет легче задействовать векторизацию (О которой насписанно ниже)</p1>

  <h2><u>Основы векторизации кода </u> </h2>

  <p1>Современные процессоры поддерживают такие инструкции как AVX, AVX2, AVX512. Данные инструкции разработанны для веркторизации вычислений что в свою очередь увеличивает производительность системы</p1>

  <p1>Пример Архитектуры SIMD (Single Instruction Stream & Multiple Data Stream) до использования векторизации и после</p1>
 <center><img src="https://raw.githubusercontent.com/VAsiaa/Fortran-Programming-Exercises/main/Pics/pic3.png" alt="Пример Архитектуры SIMD"></center>

  <p1>Порядок векторизации:</p1>
  <ol>
    <li>Формирование скалярных данных вектора (Запись данных в векторные регистры), задействуется инструкция из расширенного набора инструкция</li>
    <li>Применение к векторам векторных операций (Используется векторная АЛУ или ФПЮ и векторные регистры)</li>
    <li>Сохранение результата в векторные регистры, а затем в оперативную память</li>
  </ol>

  <p1>Условия векторизации:</p1>
  <ol>
    <li>Данные должны быть сплошными в памяти</li>
    <li>Должен обеспечиваться регулярный доступ к памяти</li>
    <li>Первый элемент каждого вектора должен быть выровнен</li>
    <li>Не должно быть перекрытий по памяти</li>
    <li>Не должно быть зависимостей итераций в виде чтение-после-записи </li>
  </ol>

  <h2>Процедуры</h2>

  <p1>Модули (Module)</p1>
  
    module NAME
    ...
    Contains
      modular procedures
    end module

  <ol>
    <li>Описываются в модулях, после оператора "contains"</li>
    <li>Обладают явным интерфейсом</li>
    <li>Имеют доступ ко всем обьектам модуля</li>
    <li>Могут содержать другие внутренние процедуры</li>
  </ol>

  <p1>Головная программа</p1>

    Program NAME
    ...
    Contains
      internal procedures
    end 

  <ol>
    <li>Описываются в головной прогрмме, после оператора "contains"</li>
    <li>Обладают явным интерфейсом</li>
    <li>Имеют доступ ко всем обьектам головной программы</li>
    <li>Не Могут содержать другие внутренние процедуры</li>
  </ol>

  <p1>Внешние процедуры</p1>

    subroutine or function
    ...
    Contains
      internal procedures
    end 

  <ol>
    <li>Описываются отдельно от головной программы или в других файлах</li>
    <li>Обладают неявным интерфейсом</li>
    <li>Обмен данными с головной программы происходит посредствам формальных параметров</li>
    <li>Могут содержать другие внутренние процедуры</li>
  </ol>

 <h2>Чистые Процедуры</h2>
 <p1>Функции никогда не должны обладать побочных эффектом (К примеру изменять глобальные переменные, вместо этого они должны обращатся к глобальным переменным только на чтение)<p1>

    pure real(R_) function Name(args)
      intent(in)    : args
      intent(out)   : args
      intent(inout) : args

      *internal args
      ...
    end function Name
  <ol>
    <li>Все формальные параметры функции имеют вид связи «intend(in)»</li>
    <li>Все формальные параметры подпрограмм имеют вид связи intend (in, out или inout)</li>
  </ol>



  <hr>
  <h1>Практические задания по предмету "Алгоритмы и стуктуры данных"</h1>
 
  <p1>Рассмотрим следующие структуры данных</p1>


<hr>
 </body>
</html>




