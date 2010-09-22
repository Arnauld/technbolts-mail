package org.technbolts.util

sealed trait RetCode
case object Ok extends RetCode
case object Ko extends RetCode