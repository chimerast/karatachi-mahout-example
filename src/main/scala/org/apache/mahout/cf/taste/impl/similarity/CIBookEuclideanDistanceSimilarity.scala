package org.apache.mahout.cf.taste.impl.similarity

import org.apache.mahout.cf.taste.common.Weighting
import org.apache.mahout.cf.taste.model.DataModel

class CIBookEuclideanDistanceSimilarity(dataModel: DataModel) extends AbstractSimilarity(dataModel, Weighting.UNWEIGHTED, false) {
  override def computeResult(n: Int, sumXY: Double, sumX2: Double, sumY2: Double, sumXYdiff2: Double): Double = {
    return 1.0 / (1.0 + sumXYdiff2);
  }
}
