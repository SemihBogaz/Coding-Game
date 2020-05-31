import math._
import scala.util._
import scala.io.StdIn._
import scala.util.control.Breaks._  

//import mutable list
import scala.collection.mutable.ListBuffer

//class for sample -> Sample(sampleId,carriedBy,rank,expertiseGain,health,costs)      
class Sample(var sampleId:Int, var carriedBy:Int,var rank:Int,var gain:Any,var health:Int,var costs:Array[Int]){
    var diag=health !=(-1)
}
class Robot(var storage:Array[Int],var target:String){
    var Carry = new ListBuffer[Sample]()

    def this(storage:Array[Int],target:String,Carry:ListBuffer[Sample]){
        this(storage,target)
        this.Carry = Carry

}
}

object Player extends App {
    val projectCount = readLine.toInt
    for(i <- 0 until projectCount) {
        val Array(a, b, c, d, e) = (readLine split " ").map (_.toInt)
    }

    def connectNgo(module:String,data:Any,position:String){
        if(position == module){
            println("CONNECT "+data)
        }else{
            println("GOTO "+module)
        }
    }


    // game loop
    while(true) {
        
        //sample list
        var samples = new ListBuffer[Sample]()

        //player list
        var robots = new ListBuffer[Robot]()

        for(i <- 0 until 2) {
            val Array(target, _eta, _score, _storageA, _storageB, _storageC, _storageD, _storageE, _expertiseA, _expertiseB, _expertiseC, _expertiseD, _expertiseE) = readLine split " "
            val eta = _eta.toInt //ignore
            val score = _score.toInt
            val storageA = _storageA.toInt
            val storageB = _storageB.toInt
            val storageC = _storageC.toInt
            val storageD = _storageD.toInt
            val storageE = _storageE.toInt
            val expertiseA = _expertiseA.toInt //ignore 
            val expertiseB = _expertiseB.toInt //ignore
            val expertiseC = _expertiseC.toInt //ignore
            val expertiseD = _expertiseD.toInt //ignore
            val expertiseE = _expertiseE.toInt //ignore
            
            robots+= new Robot(Array(storageA,storageB,storageC,storageD,storageE),target) 
           
        }
        val Array(availableA, availableB, availableC, availableD, availableE) = (readLine split " ").map (_.toInt)
        val sampleCount = readLine.toInt
        for(i <- 0 until sampleCount) {
            val Array(_sampleId, _carriedBy, _rank, expertiseGain, _health, _costA, _costB, _costC, _costD, _costE) = readLine split " "
            val sampleId = _sampleId.toInt
            val carriedBy = _carriedBy.toInt
            val rank = _rank.toInt //ignore
            val health = _health.toInt
            val costA = _costA.toInt
            val costB = _costB.toInt
            val costC = _costC.toInt
            val costD = _costD.toInt
            val costE = _costE.toInt

            //array of costs
            var  costs = Array(costA,costB,costC,costD,costE) 

            //creating a sample object in every turn and adding to the list
            var sample =new Sample(sampleId,carriedBy,rank,expertiseGain,health,costs)  
            samples+=sample    
            if(carriedBy > (-1)){
                robots(carriedBy).Carry +=sample
            }         
        }
        val me = robots(0)

        //taşımıyorsam
        if(me.Carry.isEmpty){
            connectNgo("SAMPLES",2,me.target)            
        }else{
            var sample =me.Carry(0)
            if(sample.diag){
                var need:Any = null // or ""
                breakable{
                    for(i<- 0 until 5){
                        if(me.storage(i) < sample.costs(i)){
                            need = "ABCDE"(i)
                            //println(need)
                            break()
                            }
                        }
                }
                if(need != null){
                    connectNgo("MOLECULES",need,me.target)
                    }else{
                        connectNgo("LABORATORY",sample.sampleId,me.target)
                        }
                 }
                 else{
                      connectNgo("DIAGNOSIS",sample.sampleId,me.target)
                 }
        }
        
        
        
    }
}

//Console.err.println(TargeT)