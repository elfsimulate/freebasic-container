' ########################################################################################
' 通用 HashMap 模块（2 的幂桶大小 + 位运算优化版）
' 作者：ycp, 406192297@qq.com
' 使用方式：define_hashmap(keytype, valuetype, mapname)
' License: Distributed under the MIT license.
'
' Permission is hereby granted, free of charge, to any person obtaining a copy of this
' software and associated documentation files (the "Software"), to deal in the Software
' without restriction, including without limitation the rights to use, copy, modify, merge,
' publish, distribute, sublicense, and/or sell copies of the Software, and to permit
' persons to whom the Software is furnished to do so, subject to the following conditions:

' The above copyright notice and this permission notice shall be included in all copies or
' substantial portions of the Software.

' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
' INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
' PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
' FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
' OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
' DEALINGS IN THE SOFTWARE.'
' ########################################################################################


#include once "crt/mem.bi"

'==============高性能字符串哈希函数 rapidhash ==============
Dim Shared rapid_secret(0 To 7) As ULongInt = { &H2D358DCCAA6C78A5ull, &H8BB84B93962EACC9ull,&H4B33A62ED433D4A3ull, &H4D5A2DA51DE1AA47ull, _
&HA0761D6478BD642Full,&HE7037ED1A0B428DBull,&H90ED1765281C388Cull, &HAAAAAAAAAAAAAAAAull }

Sub rapid_mum(ByRef A As ULongInt, ByRef B As ULongInt)
    Dim As ULongInt ha = (A Shr 32), hb = (B Shr 32)
    Dim As ULongInt la = A And &HFFFFFFFFull, lb = B And &HFFFFFFFFull
    Dim As ULongInt rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb
    Dim As ULongInt t = (rl + (rm0 Shl 32)), c = (IIf(t < rl, 1, 0))
    Dim As ULongInt lo = t + (rm1 Shl 32)
    c += IIf(lo < t, 1, 0)
    Dim As ULongInt hi = rh + (rm0 Shr 32) + (rm1 Shr 32) + c
    A = lo:        B = hi
End Sub

Function rapid_mix(ByVal A As ULongInt, ByVal B As ULongInt) As ULongInt
    Dim As ULongInt ha = (A Shr 32), hb = (B Shr 32)
    Dim As ULongInt la = A And &HFFFFFFFFull, lb = B And &HFFFFFFFFull
    Dim As ULongInt rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb
    Dim As ULongInt t = (rl + (rm0 Shl 32)), c = (IIf(t < rl, 1, 0))
    Dim As ULongInt lo = t + (rm1 Shl 32)
    c += IIf(lo < t, 1, 0)
    Dim As ULongInt hi = rh + (rm0 Shr 32) + (rm1 Shr 32) + c
    A = lo:        B = hi
    Function = A Xor B
End Function

Function rapid_read64(ByVal p As UByte Ptr) As ULongInt
    Dim As ULongInt v
    memcpy(@v, p, 8)
    Function = v
End Function

Function rapid_read32(ByVal p As UByte Ptr) As ULongInt
    Dim As ULong v
    memcpy(@v, p, 4)
    Function = v
End Function

Function rapidhashNano_internal( _
        ByVal key As Any Ptr, _
        ByVal lens As UInteger, _
        ByVal seed As ULongInt, _
        ByVal secret As ULongInt Ptr _
        ) As ULongInt
        Dim As UByte Ptr p = key
        seed Xor= rapid_mix(seed Xor secret[2], secret[1])
        Dim As ULongInt a = 0, b = 0
        Dim As UInteger i = lens
        If (lens <= 16) Then
            If lens >= 4 Then
                seed Xor= lens
                If lens >= 8 Then
                    Dim As UByte Ptr plast = p + lens - 8
                    a = rapid_read64(p) : b = rapid_read64(plast)
                Else
                    Dim As UByte Ptr plast = p + lens - 4
                    a = rapid_read32(p) : b = rapid_read32(plast)
                End If
            ElseIf lens > 0 Then
                a = ((CULngInt(p[0]) Shl 45) Or p[lens - 1])
                b = p[lens Shr 1]
            Else
                a = 0 : b = 0
            End If
        Else
            If i > 48 Then
                Dim As ULongInt see1 = seed, see2 = seed
                Do
                    seed = rapid_mix(rapid_read64(p)     Xor secret[0], rapid_read64(p + 8)  Xor seed)
                    see1 = rapid_mix(rapid_read64(p + 16) Xor secret[1], rapid_read64(p + 24) Xor see1)
                    see2 = rapid_mix(rapid_read64(p + 32) Xor secret[2], rapid_read64(p + 40) Xor see2)
                    p += 48 : i -= 48
                Loop While i > 48
                seed Xor= see1 : seed Xor= see2
            End If
            If i > 16 Then
                seed = rapid_mix(rapid_read64(p) Xor secret[2], rapid_read64(p + 8) Xor seed)
                If i > 32 Then
                    seed = rapid_mix(rapid_read64(p + 16) Xor secret[2], rapid_read64(p + 24) Xor seed)
                End If
            End If
            a = rapid_read64(p + i - 16) Xor i
            b = rapid_read64(p + i - 8)
        End If
        a Xor= secret[1]:    b Xor= seed
        rapid_mum(a, b)
        Function = rapid_mix(a Xor secret[7], b Xor secret[1] Xor i)
End Function

Function rapidhashNano(ByVal key As Any Ptr, ByVal keylens As UInteger, ByVal seed As ULongInt=0) As ULongInt
    Function = rapidhashNano_internal(key, keylens, seed, @rapid_secret(0))
End Function

'==============多类型哈希函数重载====================
Declare function HashFunction overload (byref key as integer) as uinteger
Declare Function HashFunction Overload (ByRef key As UInteger) As UInteger
Declare Function HashFunction Overload (ByRef key As LongInt) As UInteger
declare function HashFunction overload (byref key as ulongint) as uinteger
declare function HashFunction overload (byref key as string) as uinteger
declare function HashFunction overload (byref key as single) as uinteger
Declare Function HashFunction Overload (ByRef key As Double) As UInteger
Declare Function HashFunction Overload (ByRef key As ULong) As UInteger

function HashFunction overload (byref key as integer) as uinteger
    dim h as uinteger = cuint(key)
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h xor (h shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h 
end function

Function HashFunction Overload (ByRef key As UInteger) As UInteger
    Dim h As UInteger = key
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h xor (h shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h 
end function

Function HashFunction Overload (ByRef key As ULong) As UInteger
    Dim h As UInteger = key
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h Xor (h Shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h 
End Function

function HashFunction overload (byref key as longint) as uinteger
    dim h as ulongint = culngint(key)
    h = (h xor (h shr 30)) * &hbf58476d1ce4e5b9ull
    h = (h xor (h shr 27)) * &h94d049bb133111ebull
    h = h xor (h shr 31)
    return culng(h )
end function

function HashFunction overload (byref key as ulongint) as uinteger
    dim h as ulongint = key
    h = (h xor (h shr 30)) * &hbf58476d1ce4e5b9ull
    h = (h xor (h shr 27)) * &h94d049bb133111ebull
    h = h xor (h shr 31)
    return culng(h )
end function

Function HashFunction Overload (ByRef key As String) As UInteger
    Dim hash64 As ulongint = rapidhashNano(StrPtr(key), len(key))
    return cuint(hash64 )
end function

Function HashFunction Overload (ByRef key As Double) As uinteger
    dim u as ulongint = *cptr(ulongint ptr, @key)
    u = (u xor (u shr 32)) * &h9e3779b97f4a7c15ull
    u = (u xor (u shr 28)) * &hbf58476d1ce4e5b9ull
    u = u xor (u shr 31)
    return culng(u )
End Function

function HashFunction overload (byref key as single) as uinteger
    dim u as ulong = *cptr(ulong ptr, @key)
    u = (u xor (u shr 16)) * &h85ebca6b
    u = (u xor (u shr 13)) * &hc2b2ae35
    u = u xor (u shr 16)
    return u 
end function

' =====================================================
' 通用哈希映射宏（使用 New/Delete，2 的幂桶大小 + 位运算）
' =====================================================

#macro DEFINE_HASHMAP(KEY_TYPE, VALUE_TYPE, MAP_NAME)    
    ' 键值对结构
    type MAP_NAME##KeyValue
        key as KEY_TYPE
        value as VALUE_TYPE
    end type
    
    ' 哈希节点结构
    type MAP_NAME##Node
        key as KEY_TYPE
        value as VALUE_TYPE
        next as MAP_NAME##Node ptr
    end type

    ' 迭代器状态枚举
    enum MAP_NAME##IteratorState
        MAP_NAME##_ITERATOR_INVALID = 0
        MAP_NAME##_ITERATOR_VALID = 1
        MAP_NAME##_ITERATOR_END = 2
    end enum

    ' 哈希映射类
    type MAP_NAME
    private:
        m_buckets as MAP_NAME##Node ptr ptr
        m_bucketCount as uinteger
        m_count as uinteger
        m_loadFactor as single
        m_version as integer
        m_currentBucket as integer
        m_currentIter as MAP_NAME##Node ptr
        m_iterState as MAP_NAME##IteratorState
        m_iterVersion as integer
        
        declare function DefaultHash(byref key as KEY_TYPE) as uinteger
        declare sub Rehash(newSize as uinteger)
        declare function GetBucketIndex(byval hash as uinteger) as uinteger
        declare function NextPowerOfTwo(byval n as uinteger) as uinteger
        
    public:
        declare constructor()
        declare constructor(byval initialSize as integer, byval factor as single = 0.75)
        declare destructor()
        
        declare function Add(byref key as KEY_TYPE, byref value as VALUE_TYPE) as boolean
        declare function Get(byref key as KEY_TYPE) as VALUE_TYPE
        declare function find(byref key as KEY_TYPE, byref value as VALUE_TYPE) as boolean
        declare function Remove(byref key as KEY_TYPE) as boolean
        Declare Function ContainsKey(ByRef key As KEY_TYPE) As Boolean
        declare sub Clear()
        
        declare property Size() as uinteger
        declare property Capacity() as uinteger
        declare property LoadFactor() as single
        declare property IsEmpty() as boolean
        
        declare function MoveFirst() as boolean
        declare function MoveNext() as boolean
        declare function CurrentKey(byref key as KEY_TYPE) as boolean
        declare function CurrentValue(byref value as VALUE_TYPE) as boolean
        
        Declare Sub ForEach(ByVal callback As Sub(ByVal key As KEY_TYPE, ByVal value As VALUE_TYPE))
        declare sub ForEachPtr(byval callback as sub(byval key as KEY_TYPE ptr, byval value as VALUE_TYPE ptr))

        declare sub Optimize()
        declare sub PrintStatistics()
    end type

    ' 返回不小于 n 的 2 的幂（n > 0）
    function MAP_NAME.NextPowerOfTwo(byval n as uinteger) as uinteger
        if n <= 0 then return 1
        n -= 1
        n or= n shr 1
        n or= n shr 2
        n or= n shr 4
        n or= n shr 8
        n or= n shr 16
        return n + 1
    end function

    ' 默认构造函数
    constructor MAP_NAME()
        m_bucketCount = 16   ' 2^4
        m_buckets = callocate(m_bucketCount * sizeof(MAP_NAME##Node ptr))
        if m_buckets = 0 then
            print "Error: Failed to allocate hashmap buckets"
        end if
        m_count = 0
        m_loadFactor = 0.75
        m_version = 0
        m_currentBucket = -1
        m_currentIter = 0
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = 0
    end constructor
    
    ' 带参数的构造函数
    constructor MAP_NAME(byval initialSize as integer, byval factor as single = 0.75)
        if initialSize < 1 then initialSize = 16
        m_bucketCount = NextPowerOfTwo(initialSize)   ' 取 2 的幂
        m_buckets = callocate(m_bucketCount * sizeof(MAP_NAME##Node ptr))
        if m_buckets = 0 then
            print "Error: Failed to allocate hashmap buckets"
        end if
        m_count = 0
        m_loadFactor = factor
        m_version = 0
        m_currentBucket = -1
        m_currentIter = 0
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = 0
    end constructor

    ' 析构函数
    destructor MAP_NAME()
        Clear()
        if m_buckets <> 0 then
            deallocate(m_buckets)
            m_buckets = 0
        end if
    end destructor
    
    ' 获取桶索引（位运算优化）
    function MAP_NAME.GetBucketIndex(byval hash as uinteger) as uinteger
        return hash and (m_bucketCount - 1)
    end function
    
    ' 默认哈希函数
    function MAP_NAME.DefaultHash(byref key as KEY_TYPE) as uinteger
        return HashFunction(key)
    end function
    
    ' 优化哈希表大小（调整为 2 的幂）
    sub MAP_NAME.Optimize()
        if m_count = 0 then exit sub
        dim optimalSize as integer = NextPowerOfTwo(m_count / m_loadFactor)
        if optimalSize <> m_bucketCount then
            Rehash(optimalSize)
        end if
    end sub
    
    ' 重新哈希（新大小必须为 2 的幂）
    sub MAP_NAME.Rehash(newSize as uinteger)
        ' 确保新大小是 2 的幂
        newSize = NextPowerOfTwo(newSize)
        if newSize = m_bucketCount then exit sub
        
        ' 保存旧桶数组信息
        dim oldBuckets as MAP_NAME##Node ptr ptr = m_buckets
        dim oldBucketCount as integer = m_bucketCount
        
        ' 创建新桶数组
        dim newBuckets as MAP_NAME##Node ptr ptr = callocate(newSize * sizeof(MAP_NAME##Node ptr))
        If newBuckets = 0 Then
            print "Warning: Rehash failed due to memory allocation"
            exit sub
        end if
        
        ' 重新哈希所有节点到新桶数组
        for i as uinteger = 0 to oldBucketCount - 1
            dim current as MAP_NAME##Node ptr = oldBuckets[i]
            while current <> 0
                dim nextNode as MAP_NAME##Node ptr = current->next
                
                ' 计算新哈希位置（使用位运算）
                dim newHash as uinteger = DefaultHash(current->key)
                dim newIndex as uinteger = newHash and (newSize - 1)
                
                ' 插入到新桶（头插法）
                current->next = newBuckets[newIndex]
                newBuckets[newIndex] = current
                
                current = nextNode
            wend
        next
        
        ' 更新成员变量
        m_buckets = newBuckets
        m_bucketCount = newSize
        
        ' 释放旧的桶数组（只释放数组内存，节点已经移动）
        deallocate(oldBuckets)
        
        ' 版本号增加
        m_version += 1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
    end sub

    ' 清理所有节点
    sub MAP_NAME.Clear()
        if m_buckets = 0 then exit sub
        
        for i as uinteger = 0 to m_bucketCount - 1
            dim current as MAP_NAME##Node ptr = m_buckets[i]
            while current <> 0
                dim nextNode as MAP_NAME##Node ptr = current->next
                Delete current
                current = nextNode
            wend
            m_buckets[i] = 0
        next
        m_count = 0
        m_version += 1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
    end sub

    ' 添加/更新键值对
    function MAP_NAME.Add(byref key as KEY_TYPE, byref value as VALUE_TYPE) as boolean
        ' 检查是否需要扩容
        If m_count >= m_bucketCount * m_loadFactor Then
            Dim newSize As UInteger = m_bucketCount * 2            
            If newSize > &H40000000 Then  newSize = &H40000000
            Rehash(newSize)
        End If
        
        dim hash as uinteger = DefaultHash(key)
        dim index as uinteger = GetBucketIndex(hash)
        
        dim current as MAP_NAME##Node ptr = m_buckets[index]

        ' 检查是否已存在，如果存在则更新
        while current <> 0
            if current->key = key then
                current->value = value
                m_version += 1
                return true
            end if
            current = current->next
        wend

        ' 创建新节点
        dim newNode as MAP_NAME##Node ptr = New MAP_NAME##Node
        if newNode = 0 then return false
        
        newNode->key = key
        newNode->value = value
        newNode->next = m_buckets[index]
        m_buckets[index] = newNode
        m_count += 1
        m_version += 1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        return true
    end function

    ' 获取值
    function MAP_NAME.Get(byref key as KEY_TYPE) as VALUE_TYPE
        dim hash as uinteger = DefaultHash(key)
        dim index as uinteger = GetBucketIndex(hash)
        
        dim current as MAP_NAME##Node ptr = m_buckets[index]
        dim tmpValue as VALUE_TYPE
        While current <> 0
            If current->key = key Then
                tmpvalue = current->value
                exit while
            end if
            current = current->next
        Wend
        return tmpValue
    end function
    
    ' 查找值（返回是否存在）
    function MAP_NAME.find(byref key as KEY_TYPE, byref value as VALUE_TYPE) as boolean
        dim hash as uinteger = DefaultHash(key)
        dim index as uinteger = GetBucketIndex(hash)
        
        dim current as MAP_NAME##Node ptr = m_buckets[index]

        While current <> 0
            If current->key = key Then
                value = current->value
                return true
            end if
            current = current->next
        Wend

        return false
    end function

    ' 删除键
    function MAP_NAME.Remove(byref key as KEY_TYPE) as boolean
        dim hash as uinteger = DefaultHash(key)
        dim index as uinteger = GetBucketIndex(hash)
        
        dim current as MAP_NAME##Node ptr = m_buckets[index]
        dim prev as MAP_NAME##Node ptr = 0

        while current <> 0
            if current->key = key then
                if prev = 0 then
                    m_buckets[index] = current->next
                else
                    prev->next = current->next
                end if
                Delete current
                m_count -= 1
                m_version += 1
                m_iterState = MAP_NAME##_ITERATOR_INVALID
                return true
            end if
            prev = current
            current = current->next
        wend
        return false
    end function

    ' 检查键是否存在
    function MAP_NAME.ContainsKey(byref key as KEY_TYPE) as boolean
        dim hash as uinteger = DefaultHash(key)
        Dim index As uInteger = GetBucketIndex(hash)
        
        dim current as MAP_NAME##Node ptr = m_buckets[index]
        while current <> 0
            if current->key = key then return true
            current = current->next
        wend
        return false
    end function

    ' 获取元素数量
    property MAP_NAME.Size() as uinteger
        return m_count
    end property
    
    ' 获取桶容量
    property MAP_NAME.Capacity() as uinteger
        return m_bucketCount
    end property
    
    ' 获取当前负载因子
    property MAP_NAME.LoadFactor() as single
        if m_bucketCount = 0 then return 0.0
        return m_count / m_bucketCount
    end property
    
    ' 是否为空
    property MAP_NAME.IsEmpty() as boolean
        return m_count = 0
    end property

    ' 移动到第一个元素
    function MAP_NAME.MoveFirst() as boolean
        if m_iterVersion <> m_version then
            m_iterState = MAP_NAME##_ITERATOR_INVALID
        end if
        
        m_currentBucket = -1
        m_currentIter = 0
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = m_version
        
        return MoveNext()
    end function

    ' 移动到下一个元素
    function MAP_NAME.MoveNext() as boolean
        if m_iterVersion <> m_version then
            m_iterState = MAP_NAME##_ITERATOR_INVALID
            return false
        end if
        
        if m_iterState = MAP_NAME##_ITERATOR_VALID and m_currentIter <> 0 then
            m_currentIter = m_currentIter->next
            if m_currentIter <> 0 then
                return true
            else
                m_iterState = MAP_NAME##_ITERATOR_END
            end if
        end if

        while m_currentBucket < m_bucketCount - 1
            m_currentBucket += 1
            m_currentIter = m_buckets[m_currentBucket]
            if m_currentIter <> 0 then
                m_iterState = MAP_NAME##_ITERATOR_VALID
                return true
            end if
        wend
        
        m_iterState = MAP_NAME##_ITERATOR_END
        m_currentIter = 0
        return false
    end function

    ' 获取当前键
    function MAP_NAME.CurrentKey(byref key as KEY_TYPE) as boolean
        if m_iterState = MAP_NAME##_ITERATOR_VALID and m_currentIter <> 0 and m_iterVersion = m_version then
            key = m_currentIter->key
            return true
        end if
        return false
    end function

    ' 获取当前值
    function MAP_NAME.CurrentValue(byref value as VALUE_TYPE) as boolean
        if m_iterState = MAP_NAME##_ITERATOR_VALID and m_currentIter <> 0 and m_iterVersion = m_version then
            value = m_currentIter->value
            return true
        end if
        return false
    end function
    
    ' 遍历回调函数
    sub MAP_NAME.ForEach(byval callback as sub(byval key as KEY_TYPE, byval value as VALUE_TYPE))
        dim ver as integer = m_version
        for i as uinteger = 0 to m_bucketCount - 1
            if ver <> m_version then exit for
            dim current as MAP_NAME##Node ptr = m_buckets[i]
            while current <> 0 and ver = m_version
                callback(current->key, current->value)
                current = current->next
            wend
        next
    end sub
    
    ' 遍历回调函数（指针版本）
    sub MAP_NAME.ForEachPtr(byval callback as sub(byval key as KEY_TYPE ptr, byval value as VALUE_TYPE ptr))
        dim ver as integer = m_version
        for i as uinteger = 0 to m_bucketCount - 1
            if ver <> m_version then exit for
            dim current as MAP_NAME##Node ptr = m_buckets[i]
            while current <> 0 and ver = m_version
                callback(@current->key, @current->value)
                current = current->next
            wend
        next
    End Sub
 
    
    ' 打印统计信息
    Sub MAP_NAME.PrintStatistics()
        print "=== HashMap Statistics ==="
        print "Size: " & m_count
        print "Capacity: " & m_bucketCount
        print "Load Factor: " & LoadFactor
        print "Threshold: " & m_loadFactor
        
        dim maxChain as integer = 0
        dim emptyBuckets as integer = 0
        dim totalChainLength as integer = 0
        
        for i as uinteger = 0 to m_bucketCount - 1
            dim length as integer = 0
            dim current as MAP_NAME##Node ptr = m_buckets[i]
            while current <> 0
                length += 1
                current = current->next
            wend
            if length = 0 then
                emptyBuckets += 1
            else
                totalChainLength += length
                if length > maxChain then maxChain = length
            end if
        next        
        print "Empty buckets: " & emptyBuckets & " (" & (emptyBuckets * 100 \ m_bucketCount) & "%)"
        print "Max chain length: " & maxChain
        if (m_bucketCount - emptyBuckets) > 0 then
            print "Avg chain length (non-empty): " & (totalChainLength / (m_bucketCount - emptyBuckets))
        end if
        print "=========================="
    end sub
    
#endmacro
