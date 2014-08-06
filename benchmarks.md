_Benchmark monoid appends.append_

    (spacedust -> IList[Int]): 6.848
    (spacedust -> List[Int]): 23.458
    (spacedust -> DList[Int]): 6.296
    (spacedust -> Vector[Int]): 63.219
    (spacedust -> Map[String, Int]): 30.504
    (spacedust -> String ==>> Int): 14.948
    (spacedust -> ISet[Int]): 5.795
    (spacedust -> Set[Int]): 61.045
	
_Benchmark monoid appends.monoid prepend_

    (spacedust -> IList[Int]): 11.253
    (spacedust -> List[Int]): 23.088
    (spacedust -> DList[Int]): 9.875
    (spacedust -> Vector[Int]): 78.074
    (spacedust -> Map[String, Int]): 44.475
    (spacedust -> String ==>> Int): 15.162
    (spacedust -> ISet[Int]): 6.294
    (spacedust -> Set[Int]): 62.405
	
_Benchmark monoid appends.create from list_

    (spacedust -> IList[Int]): 87.192
    (spacedust -> List[Int]): 64.398
    (spacedust -> DList[Int]): 0.008
    (spacedust -> Vector[Int]): 72.106
    (spacedust -> Map[String, Int]): 1183.22
    (spacedust -> String ==>> Int): 1669.424
    (spacedust -> ISet[Int]): 1828.213
    (spacedust -> Set[Int]): 915.879
	
_Benchmark monoid appends.append two big lists_

    (spacedust -> IList[Int]): 47.875
    (spacedust -> List[Int]): 33.598
    (spacedust -> DList[Int]): 0.006
    (spacedust -> Vector[Int]): 143.001
    (spacedust -> Map[String, Int]): 836.154
    (spacedust -> String ==>> Int): 415.293
    (spacedust -> ISet[Int]): 93.497
    (spacedust -> Set[Int]): 260.615
	
_Benchmark list things.filter_

    (listing -> List[Int]): 43.262
    (listing -> IList[Int]): 84.198
	
_Benchmark list things.collect_

    (listing -> List[Int]): 84.52
    (listing -> IList[Int]): 55.299
	
_Benchmark list things.reverse_

    (listing -> List[Int]): 30.117
    (listing -> IList[Int]): 37.136
	
_Benchmark list things.foldLeft_

    (listing -> List[Int]): 28.495
    (listing -> IList[Int]): 31.883
	
_Benchmark list things.foldRight_

    (listing -> List[Int]): 63.037
    (listing -> IList[Int]): 71.779
	
_Benchmark list things.drop_

    (listing -> List[Int]): 22.196
    (listing -> IList[Int]): 14.892
	
_Benchmark list things.take_

    (listing -> List[Int]): 29.079
    (listing -> IList[Int]): 44.085
	
_Benchmark list things.find_

    (listing -> List[Int]): 32.509
    (listing -> IList[Int]): 28.92
	
_Benchmark list things.map_

    (listing -> List[Int]): 208.98
    (listing -> IList[Int]): 148.76
	
_Benchmark list things.flatMap_

    (listing -> List[Int]): 277.384
    (listing -> IList[Int]): 136.703
	
_Benchmark traversury.sequence_

    (traversury -> List[Option]): 910.013
    (traversury -> List[Maybe]): 1077.379

